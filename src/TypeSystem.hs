module TypeSystem where

import MungoParser
import AST
import Data.Maybe
import Control.Applicative

-- EnvTF
type FieldTypeEnv = [(FieldName, Type)] 

-- Λ
type ObjectFieldTypeEnv = [(ObjectName, ((ClassName, Type), FieldTypeEnv))]

-- EnvTO
type ObjectTypeEnv = [(ObjectName, Typestate)]

-- EnvTS
type ParameterStackTypeEnv = [(ObjectName, (ParameterName, Type))]

-- Δ
type Delta = (ObjectTypeEnv, ParameterStackTypeEnv)

-- Ω
type LabelEnv = [(LabelName, (ObjectFieldTypeEnv, Delta))]

--fieldTypeToType :: FieldType -> Type
--fieldTypeToType (BaseFieldType b) = (BType b)
--fieldTypeToType (ClassFieldType ts) = (CType (ComplexClassType ts))

assert True _ = pure ()
assert False err = fail err

assert' True  _   res = res
assert' False err res = fail err
 
lin :: Type -> Bool
lin (CType (cname, usage)) = usage /= UsageEnd
lin _ = False

getField cls classname fieldname = 
    let clazz = head $ filter (\c -> cname c == classname) cls in
        ftype . head $ filter (\f -> fname f == fieldname) $ cfields clazz

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err Nothing  = Left err
maybeToEither err (Just x) = Right x

(~~) = maybeToEither

maybeEitherToEither :: String -> Maybe (Either String a) -> Either String a
maybeEitherToEither err Nothing  = Left err
maybeEitherToEither err (Just x) = x

maybeLast :: [a] -> Maybe a
maybeLast xs = if (length xs) > 0 then Just (last xs) else Nothing

lastDelta :: Delta -> Maybe (ObjectName, (ParameterName, Type))
lastDelta = maybeLast . snd

initDelta :: Delta -> Delta
initDelta (x, y) = (x, init y)

addDelta :: Delta -> (ObjectName, (ParameterName, Type)) -> Delta
addDelta (x, y) el = (x, y ++ [el])

with = addDelta 

envLookup :: [(String, a)] -> String -> Maybe a
envLookup = flip lookup

without :: [(String, a)] -> String -> [(String, a)]
without env name = filter ((/= name) . fst) env

replaceFieldType :: FieldTypeEnv -> String -> Type -> FieldTypeEnv
replaceFieldType env name ntype = map (\(n, t) -> if n == name then (n, ntype) else (n, t)) env

updateLambda :: ObjectFieldTypeEnv -> FieldName -> FieldName -> Type -> ObjectFieldTypeEnv
updateLambda lambda oname field ntype = 
    let (c, env) = fromJust $ envLookup lambda oname
        newEnv = replaceFieldType env field ntype
    in 
        (oname, (c, newEnv)):(without lambda oname)

getLambdaField lambda oname fldname = do
    ((clazzname, t), env) <- envLookup lambda oname
    envLookup env fldname

agree :: FieldType -> Type -> Bool
agree (BaseFieldType b1) (BType b2) = b1 == b2
agree (ClassFieldType cn1) (CType (cn2, _)) = cn1 == cn2
agree _ _ = False

findCls :: [Class] -> String -> Maybe Class
findCls [] _ = Nothing
findCls (cls@(Class cn _ _ _):clss) name = if cn == name then Just cls else findCls clss name

checkExpression :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkExpression cls lambda delta omega e =  checkTNew  cls lambda delta omega e
                                  <|> checkTFld  cls lambda delta omega e
                                  <|> checkTUnit cls lambda delta omega e
                                  <|> checkTBot  cls lambda delta omega e
                                  <|> checkTSeq  cls lambda delta omega e
                                  <|> checkTBool cls lambda delta omega e
                                  <|> checkTIf   cls lambda delta omega e
                                  <|> checkTRet  cls lambda delta omega e
                                  <|> checkTObj  cls lambda delta omega e
                                  <|> checkTLab  cls lambda delta omega e
                                  <|> checkTCon  cls lambda delta omega e

checkTNew cls lambda delta omega (ExprNew cn) = 
    let foundCls = findCls cls cn in
        case foundCls of 
            Just (Class clsname usage _ _) -> Just $ Right (CType (clsname, usage), lambda, delta, omega)
            Nothing -> Just $ Left "Invalid class name in new"
checkTNew cls lambda delta omega _ = Nothing

checkTFld' :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTFld' cls lambda delta omega (ExprAssign fname e) = do
    let lstEl = lastDelta delta
    (oname, (x, ftype)) <- fromMaybe (Left "Wrong stack size in TFld") $ Right <$> lstEl
    
    -- C = lambda(o).class
    ((cname, _), env) <- "error" ~~ envLookup lambda oname
    
    -- e : t'
    (t', lambda', delta', omega') <- maybeEitherToEither "This *will* never happen -Mikkel 2019" $ checkExpression cls lambda delta omega e
    
    -- agree(C.fields(f), t')
    let a = agree (getField cls cname fname) t' 

    -- not lin(t)
    t <- "Cannot find final type" ~~ getLambdaField lambda' oname fname
    let nl = not $ lin t

    ((cname, typelookup), envlookup) <- "Field does not exist in lambda" ~~ envLookup lambda' oname
    let lstEl' = lastDelta delta'

    if not a then Left "Field types does not agree" else
        if not nl then Left "Field does not have a linear type" else
            Right (BType VoidType, updateLambda lambda' oname fname t', delta', omega')


checkTFld :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTFld cls lambda delta omega exp@(ExprAssign fname e) = 
    Just $ checkTFld' cls lambda delta omega exp 
checkTFld cls lambda delta omega _ = Nothing

checkTUnit :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTUnit cls lambda delta omega ExprUnit = Just $ Right (BType VoidType, lambda, delta, omega)
checkTUnit cls lambda delta omega _ = Nothing 

checkTBot :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTBot cls lambda delta omega ExprNull = Just $ Right (BotType, lambda, delta, omega)
checkTBot cls lambda delta omega _ = Nothing 

checkTBool :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTBool cls lambda delta omega (ExprBoolConst _) = Just $ Right (BType BoolType, lambda, delta, omega)
checkTBool cls lambda delta omega _ = Nothing 

checkTSeq :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTSeq cls lambda delta omega (ExprSeq e1 e2) = do
    r <- checkExpression cls lambda delta omega e1
    case r of
        Left err -> return $ Left err
        Right (t, lambda', delta', omega') -> 
            assert' (not (lin t)) "Linear value dropped in TSeq" $ do
                checkExpression cls lambda' delta' omega' e2
checkTSeq cls lambda delta omega _ = Nothing

checkTRet' :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTRet' cls lambda delta omega (ExprReturn e)= do
    let lastEl = lastDelta delta
    (o, s) <- fromMaybe (Left "Wrong stack size in TRet") $ Right <$> lastEl
    (t, lambda', delta', omega') <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda (initDelta delta) omega e
    let lastEl' = lastDelta delta'
    (o', (x, t')) <- fromMaybe (Left "Wrong stack size in TRet") $ Right <$> lastEl'
    assert (lin t') "t' must be terminated in TRet"
    Right (t, lambda', (initDelta delta') `with` (o, s), omega')

checkTRet :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTRet cls lambda delta omega (ExprReturn e) = do
    Just $ checkTRet' cls lambda delta omega e 
checkTRet cls lambda delta omega _ = Nothing

checkTIf' :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTIf' cls lambda delta omega (ExprIf e e' e'') = do
    (t, lambda'', delta'', omega'') <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda delta omega e
    assert' (t == BType BoolType) "If-case expression must evaluate to bool" $ do
    (t', lambda1, delta1, omega1) <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda'' delta'' omega'' e'
    (t'', lambda2, delta2, omega2) <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda'' delta'' omega'' e''
    assert' (t' == t'') "Types of branches in if does not match" $ do
    assert' (lambda1 == lambda2) "Lambda does not match in if" $ do
    assert' (delta1 == delta2) "Deltas do not match in if" $ do
    assert' (omega1 == omega2) "Omegas do not match in if" $ do
    Right (t', lambda1, delta1, omega1)

    

checkTIf :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTIf cls lambda delta omega e@(ExprIf e1 e2 e3) = 
    Just $ checkTIf' cls lambda delta omega e
checkTIf cls lambda delta omega _ = Nothing

checkTObj :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTObj cls lambda (envTo, envTs) omega (ExprObjectName o) = do
    let t = envLookup envTo o
    case t of
        Nothing -> Just $ Left "Could not find o in envTo"
        Just typestate -> Just $ Right ((CType typestate), lambda, ((envTo `without` o), envTs), omega)
checkTObj cls lambda delta omega _ = Nothing

checkTLab' :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTLab' cls lambda delta omega (ExprLabel label e) = do
    let binding = envLookup omega label
    case binding of
        Nothing -> Left "Could not find label in label environment"
        Just (lambda', delta') -> 
            assert' (lambda == lambda') "Lambdas do not match" $ do
            assert' (delta == delta') "Deltas do not match" $ do
            (t, lambda'', delta'', omega'') <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda' delta' (omega `without` label) e
            assert' (lambda'' == lambda') "Lambdas do not match (2)" $ do
            assert' (delta'' == delta') "Deltas do not match (2)" $ do
            assert' (t == BType VoidType) "Wrong value of labelled expression" $ do
            Right (t, lambda'', delta'', omega)
            

checkTLab :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTLab cls lambda delta omega exp@(ExprLabel label e) = 
    Just $ checkTLab' cls lambda delta omega exp
checkTLab cls lambda delta omega _ = Nothing

checkTCon :: [Class] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
checkTCon cls lambda delta omega (ExprContinue label) = do
    let binding = envLookup omega label
    case binding of 
        Nothing -> Just $ Left "No binding for label found in continue expression"
        Just (lambda', delta') -> 
            assert' (lambda == lambda') "Lambdas do not match" $ do
            assert' (delta == delta') "Deltas do not match" $ do
            Just $ Right (BType VoidType, lambda, delta, omega)
checkTCon cls lambda delta omega _ = Nothing

-- TCallF
-- TCallP
-- TSwP
-- TSwF
-- TLit
-- TLinPar
-- TNoLPar
-- TLinFld
-- TNoLFld