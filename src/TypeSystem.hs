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

(°) = addDelta 

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

checkExpression :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkExpression cls lambda delta e =  checkTNew  cls lambda delta e
                                  <|> checkTFld  cls lambda delta e
                                  <|> checkTUnit cls lambda delta e
                                  <|> checkTBot  cls lambda delta e
                                  <|> checkTSeq  cls lambda delta e
                                  <|> checkTBool cls lambda delta e
                                  <|> checkTIf   cls lambda delta e
                                  <|> checkTRet  cls lambda delta e
                                  <|> checkTObj  cls lambda delta e

checkTNew cls lambda delta (ExprNew cn) = 
    let foundCls = findCls cls cn in
        case foundCls of 
            Just (Class clsname usage _ _) -> Just $ Right (CType (clsname, usage), lambda, delta)
            Nothing -> Just $ Left "Invalid class name in new"
checkTNew cls lambda delta _ = Nothing

checkTFld' :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTFld' cls lambda delta (ExprAssign fname e) = do
    let lstEl = lastDelta delta
    (oname, (x, ftype)) <- fromMaybe (Left "Wrong stack size in TFld") $ Right <$> lstEl
    
    -- C = lambda(o).class
    ((cname, _), env) <- "error" ~~ envLookup lambda oname
    
    -- e : t'
    (t', lambda', delta') <- maybeEitherToEither "This *will* never happen -Mikkel 2019" $ checkExpression cls lambda delta e
    
    -- agree(C.fields(f), t')
    let a = agree (getField cls cname fname) t' 

    -- not lin(t)
    t <- "Cannot find final type" ~~ getLambdaField lambda' oname fname
    let nl = not $ lin t

    ((cname, typelookup), envlookup) <- "Field does not exist in lambda" ~~ envLookup lambda' oname
    let lstEl' = lastDelta delta'

    if not a then Left "Field types does not agree" else
        if not nl then Left "Field does not have a linear type" else
            Right (BType VoidType, updateLambda lambda' oname fname t', delta')


checkTFld :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTFld cls lambda delta exp@(ExprAssign fname e) = 
    Just $ checkTFld' cls lambda delta exp 
checkTFld cls lambda delta _ = Nothing

checkTUnit :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTUnit cls lambda delta ExprUnit = Just $ Right (BType VoidType, lambda, delta)
checkTUnit cls lambda delta _ = Nothing 

checkTBot :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTBot cls lambda delta ExprNull = Just $ Right (BotType, lambda, delta)
checkTBot cls lambda delta _ = Nothing 

checkTBool :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTBool cls lambda delta (ExprBoolConst _) = Just $ Right (BType BoolType, lambda, delta)
checkTBool cls lambda delta _ = Nothing 

checkTSeq :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTSeq cls lambda delta (ExprSeq e1 e2) = do
    r <- checkExpression cls lambda delta e1
    case r of
        Left err -> return $ Left err
        Right (t, lambda', delta') -> 
            assert' (not (lin t)) "Linear value dropped in TSeq" $ do
                checkExpression cls lambda' delta' e2
checkTSeq cls lambda delta _ = Nothing

checkTRet' :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTRet' cls lambda delta (ExprReturn e)= do
    let lastEl = lastDelta delta
    (o, s) <- fromMaybe (Left "Wrong stack size in TRet") $ Right <$> lastEl
    (t, lambda', delta') <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda (initDelta delta) e
    let lastEl' = lastDelta delta'
    (o', (x, t')) <- fromMaybe (Left "Wrong stack size in TRet") $ Right <$> lastEl'
    assert (lin t') "t' must be terminated in TRet"
    Right (t, lambda', (initDelta delta') ° (o, s))

checkTRet :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTRet cls lambda delta (ExprReturn e) = do
    Just $ checkTRet' cls lambda delta e 
checkTRet cls lambda delta _ = Nothing

checkTIf' :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTIf' cls lambda delta (ExprIf e e' e'') = do
    (t, lambda'', delta'') <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda delta e
    assert' (t == BType BoolType) "If-case expression must evaluate to bool" $ do
    (t', lambda1, delta1) <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda'' delta'' e'
    (t'', lambda2, delta2) <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls lambda'' delta'' e''
    assert' (t' == t'') "Types of branches in if does not match" $ do
    assert' (lambda1 == lambda2) "Lambda does not match in if" $ do
    assert' (delta1 == delta2) "Deltas do not match in if" $ do
    Right (t', lambda1, delta)

    

checkTIf :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTIf cls lambda delta e@(ExprIf e1 e2 e3) = 
    Just $ checkTIf' cls lambda delta e
checkTIf cls lambda delta _ = Nothing

checkTObj :: [Class] -> ObjectFieldTypeEnv -> Delta -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta))
checkTObj cls lambda (envTo, envTs) (ExprObjectName o) = do
    let t = envLookup envTo o
    case t of
        Nothing -> Just $ Left "Could not find o in envTo"
        Just typestate -> Just $ Right ((CType typestate), lambda, ((envTo `without` o), envTs))
checkTObj cls lambda delta _ = Nothing

-- TCallF
-- TCallP
-- TSwP
-- TSwF
-- TLab
-- TCon
-- TLit
-- TLinPar
-- TNoLPar
-- TLinFld
-- TNoLFld