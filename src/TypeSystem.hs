module TypeSystem where

import MungoParser
import AST
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Arrow (second)
import Data.List (sort, intersperse)
import Data.Either (partitionEithers)

-- EnvTF
type FieldTypeEnv = [(FieldName, Type)] 


(===) :: FieldTypeEnv -> FieldTypeEnv -> Bool
((n1, BType b1):xs1)     === ((n2, BType b2):xs2)     = n1 == n2 && b1 == b2 && xs1 == xs2
((n1, BotType ):xs1)     === ((n2, BotType):xs2)      = n1 == n2 && True && xs1 == xs2
((n1, CType c1):xs1)     === ((n2, CType c2):xs2)     = n1 == n2 && c1 == c2 && xs1 == xs2
((n1, BotType) :xs1)     === ((n2, CType (_, _, u)):xs2) = n1 == n2 && (current u) == UsageEnd && xs1 == xs2
((n1, CType (_, _, u)):xs1) === ((n2, BotType):xs2)      = n1 == n2 && (current u) == UsageEnd && xs1 == xs2
[]                       === []                       = True
_                        === _                        = False 
   





-- Θ
type RecursiveEnv = [(UsageVarName, FieldTypeEnv)]

-- Λ
type ObjectFieldTypeEnv = [(ObjectName, ((ClassName, Type), FieldTypeEnv))]

-- EnvTO
type ObjectTypeEnv = [(ObjectName, Typestate)]

-- EnvT, EqS
type ParameterStackTypeEnv = [(ObjectName, (ParameterName, Type))]

-- Δ
type Delta = (ObjectTypeEnv, ParameterStackTypeEnv)

-- Ω
type LabelEnv = [(LabelName, (ObjectFieldTypeEnv, Delta))]

type ExprCheck = [Class] -> [EnumDef] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Maybe (Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv))
type ExprCheckInternal = [Class] -> [EnumDef] -> ObjectFieldTypeEnv -> Delta -> LabelEnv -> Expression -> Either String (Type, ObjectFieldTypeEnv, Delta, LabelEnv)
type UsageCheck = [Class] -> [EnumDef] ->RecursiveEnv -> FieldTypeEnv -> Class -> Usage -> Maybe (Either String (Maybe FieldTypeEnv))


{--assert True _ = pure ()
assert False err = fail err

assert True _ = pure ()
assert False err = fail err --}

head' (x:xs) = x
head' []     = head' []


assert' :: Bool -> String -> Either String a -> Either String a
assert' True  _   res = res
assert' False err res = Left err

assertM' :: Bool -> String -> Maybe a -> Maybe a 
assertM' True  _   res = res
assertM' False err res = fail err



exists :: Eq b => [(b, a)] -> b -> Bool
exists lst b = not $ null $ filter ((== b) . fst) lst


transitions :: Usage -> [(String, Usage)]
transitions u = map toUsage $ transitions' (recursiveUsages u) (current u) 
    where recursives = recursiveUsages u
          toUsage :: (String, UsageImpl) -> (String, Usage)
          toUsage = second ((flip Usage) recursives)

transitions' :: [(String, UsageImpl)] -> UsageImpl ->  [(String, UsageImpl)]
transitions' recU UsageEnd             = []
transitions' recU (UsageBranch lst)    = lst
transitions' recU (UsageChoice lst)    = lst
transitions' recU (UsageVariable str) = 
   fromMaybe [] $ transitions' recU <$> (recU `envLookup` str)

filterUsages trans lst = map snd $ filter (\(l, u) -> l == trans) lst
 
lin :: Type -> Bool
lin (CType (cname, _, usage)) = current usage /= UsageEnd
lin _ = False


findEnum :: [EnumDef] -> LabelName -> Maybe String
findEnum [] litteral = Nothing
findEnum ((EnumDef name litterals):es) litteral = if (any (== litteral) litterals) then Just name else findEnum es litteral

getField cls classname fieldname = 
    let clazz = head $ filter (\c -> cname c == classname) cls in
        ftype . head $ filter (\f -> fname f == fieldname) $ cfields clazz

getMethod cls classname methodname = 
    let clazz = head $ filter (\c -> cname c == classname) cls in
        head $ filter (\m -> mname m == methodname) $ cmethods clazz

lookupLambda :: ObjectFieldTypeEnv -> ObjectName -> FieldName -> Maybe Type
lookupLambda lambda name field = do
    (c, flds) <- envLookup lambda name
    envLookup flds field



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

updateLambda :: ObjectFieldTypeEnv -> ObjectName -> FieldName -> Type -> ObjectFieldTypeEnv
updateLambda lambda oname field ntype = 
    let (c, env) = fromJust $ envLookup lambda oname
        newEnv = replaceFieldType env field ntype
    in 
        (oname, (c, newEnv)):(without lambda oname)

getLambdaField lambda oname fldname = do
    ((clazzname, t), env) <- envLookup lambda oname
    envLookup env fldname

agree :: FieldType -> Type -> Bool
agree (BaseFieldType b1) (BType b2)         = b1 == b2
agree (ClassFieldGen cn1 gen1) (CType (cn2, gen2, _) ) = cn1 == cn2 && gen1 == gen2
agree (ClassFieldGen cn1 gen) (BotType)        = True
agree _ _ = False

findCls :: [Class] -> String -> Maybe Class
findCls [] _ = Nothing
findCls (cls@(Class cn _ _ _ _):clss) name = if cn == name then Just cls else findCls clss name

checkExpression :: ExprCheck
checkExpression cls enums lambda delta omega e = 
                                      checkTFld    cls enums lambda delta omega e
                                  <|> checkTNew    cls enums lambda delta omega e
                                  <|> checkTUnit   cls enums lambda delta omega e
                                  <|> checkTBot    cls enums lambda delta omega e
                                  <|> checkTSeq    cls enums lambda delta omega e
                                  <|> checkTBool   cls enums lambda delta omega e
                                  <|> checkTIf     cls enums lambda delta omega e
                                  <|> checkTRet    cls enums lambda delta omega e
                                  <|> checkTObj    cls enums lambda delta omega e
                                  <|> checkTLab    cls enums lambda delta omega e
                                  <|> checkTCon    cls enums lambda delta omega e
                                  <|> checkTLit    cls enums lambda delta omega e
                                  <|> checkTCallF  cls enums lambda delta omega e
                                  <|> checkTCallP  cls enums lambda delta omega e
                                  <|> checkTSwP    cls enums lambda delta omega e
                                  <|> checkTSwF    cls enums lambda delta omega e
                                  <|> checkTParRef cls enums lambda delta omega e
                                  <|> checkTFldRef cls enums lambda delta omega e
                                  <|> (Just . Left $ "failed to parse " ++ show e)

checkTNew cls enums lambda delta omega (ExprNew cn gen) = 
    let foundCls = findCls cls cn 
    in case foundCls of 
            Just (Class clsname _ usage _ _) -> 
                Just $ Right (CType (clsname, gen, usage), lambda, delta, omega)
            Nothing -> Just $ Left "Invalid class name in new"
checkTNew cls enums lambda delta omega _ = Nothing

checkTFld' :: ExprCheckInternal
checkTFld' cls enums lambda delta omega (ExprAssign fname e) = do
    let lstEl = lastDelta delta
    (oname, (x, ftype)) <- fromMaybe (Left "Wrong stack size in TFld") $ Right <$> lstEl
    
    -- C = lambda(o).class
    ((cname, _), env) <- "error" ~~ envLookup lambda oname
    
    -- e : t'
    (t', lambda', delta', omega') <- maybeEitherToEither "This *will* never happen -Mikkel 2019" $ checkExpression cls enums lambda delta omega e
    
    -- agree(C.fields(f), t')
    let fieldType = (getField cls cname fname) 
    let a = agree (getField cls cname fname) t' 

    -- not lin(t)
    t <- "Cannot find final type" ~~ getLambdaField lambda' oname fname
    let nl = not $ lin t

    ((cname, typelookup), envlookup) <- "Field does not exist in lambda" ~~ envLookup lambda' oname
    let lstEl' = lastDelta delta'

    if not a then Left "Field types does not agree" else
        if not nl then Left "Field does not have a linear type" else
            Right (BType VoidType, updateLambda lambda' oname fname t', delta', omega')


checkTFld :: ExprCheck
checkTFld cls enums lambda delta omega exp@(ExprAssign fname e) = 
    Just $ checkTFld' cls enums lambda delta omega exp 
checkTFld cls enums lambda delta omega _ = Nothing

checkTUnit :: ExprCheck
checkTUnit cls enums lambda delta omega ExprUnit = Just $ Right (BType VoidType, lambda, delta, omega)
checkTUnit cls enums lambda delta omega _ = Nothing 

checkTBot :: ExprCheck
checkTBot cls enums lambda delta omega ExprNull = Just $ Right (BotType, lambda, delta, omega)
checkTBot cls enums lambda delta omega _ = Nothing 

checkTBool :: ExprCheck
checkTBool cls enums lambda delta omega (ExprBoolConst _) = Just $ Right (BType BoolType, lambda, delta, omega)
checkTBool cls enums lambda delta omega _ = Nothing 

checkTSeq :: ExprCheck
checkTSeq cls enums lambda delta omega (ExprSeq e1 e2) = do
    r <- checkExpression cls enums lambda delta omega e1
    case r of
        Left err -> return $ Left err
        Right (t, lambda', delta', omega') -> 
            assertM' (not (lin t)) "Linear value dropped in TSeq" $ do
                checkExpression cls enums lambda' delta' omega' e2 
checkTSeq cls enums lambda delta omega _ = Nothing

checkTRet' :: ExprCheckInternal
checkTRet' cls enums lambda delta omega (ExprReturn e)= do
    let lastEl = lastDelta delta
    (o, s) <- fromMaybe (Left "Wrong stack size in TRet") $ Right <$> lastEl
    (t, lambda', delta', omega') <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls enums lambda (initDelta delta) omega e
    let lastEl' = lastDelta delta'
    (o', (x, t')) <- fromMaybe (Left "Wrong stack size in TRet") $ Right <$> lastEl'
    assert' (lin t') "t' must be terminated in TRet" $ do
    Right (t, lambda', (initDelta delta') `with` (o, s), omega')

checkTRet :: ExprCheck
checkTRet cls enums lambda delta omega (ExprReturn e) = do
    Just $ checkTRet' cls enums lambda delta omega e 
checkTRet cls enums lambda delta omega _ = Nothing

checkTIf' :: ExprCheckInternal
checkTIf' cls enums lambda delta omega (ExprIf e e' e'') = do
    (t, lambda'', delta'', omega'') <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls enums lambda delta omega e
    assert' (t == BType BoolType) "If-case expression must evaluate to bool" $ do
    (t', lambda1, delta1, omega1) <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls enums lambda'' delta'' omega'' e'
    (t'', lambda2, delta2, omega2) <- maybeEitherToEither "Could not typecheck returned expression" $ checkExpression cls enums lambda'' delta'' omega'' e''
    assert' (t' == t'') "Types of branches in if does not match" $ do
    assert' (lambda1 == lambda2) "Lambda does not match in if" $ do
    assert' (delta1 == delta2) "Deltas do not match in if" $ do
    assert' (omega1 == omega2) "Omegas do not match in if" $ do
    Right (t', lambda1, delta1, omega1)

    

checkTIf :: ExprCheck
checkTIf cls enums lambda delta omega e@(ExprIf e1 e2 e3) = 
    Just $ checkTIf' cls enums lambda delta omega e
checkTIf cls enums lambda delta omega _ = Nothing

checkTObj :: ExprCheck
checkTObj cls enums lambda (envTo, envTs) omega (ExprObjectName o) = do
    let t = envLookup envTo o
    case t of
        Nothing -> Just $ Left "Could not find o in envTo"
        Just typestate -> Just $ Right ((CType typestate), lambda, ((envTo `without` o), envTs), omega)
checkTObj cls enums lambda delta omega _ = Nothing

checkTLab' :: ExprCheckInternal
checkTLab' cls enums lambda delta omega (ExprLabel label e) = 
    case envLookup omega label of
        Just _  -> Left $ "Label " ++ label ++ " already defined"
        Nothing -> do
            (t, lambda', delta', omega') <- maybeEitherToEither ("could not typecheck " ++ label ++ " expression" )  $ checkExpression cls enums lambda delta ((label, (lambda, delta)) :omega ) e
            assert' (lambda == lambda') "Lambdas do not match (2)" $ do
            assert' (delta == delta') "Deltas do not match (2)" $ do
            assert' (t == BType VoidType) "Wrong value of labelled expression" $ do
            Right (t, lambda', delta', omega)

checkTLab :: ExprCheck
checkTLab cls enums lambda delta omega exp@(ExprLabel label e) = 
    Just $ checkTLab' cls enums lambda delta omega exp
checkTLab cls enums lambda delta omega _ = Nothing

checkTCon :: ExprCheck
checkTCon cls enums lambda delta omega (ExprContinue label) = do
    let binding = envLookup omega label
    case binding of 
        Nothing -> Just $ Left "No binding for label found in continue expression"
        Just (lambda', delta') -> 
            Just $ do
                assert' (lambda == lambda') "Lambdas do not match" $ do
                assert' (delta == delta') "Deltas do not match" $ do
                Right (BType VoidType, lambda, delta, omega)
checkTCon cls enums lambda delta omega _ = Nothing

checkTLit :: ExprCheck
checkTLit cls enums lambda delta omega (ExprLitteral litteral) = do
    let enum = findEnum enums litteral
    case enum of 
        Nothing -> Just $ Left "No matching enum for label"
        Just e -> Just $ Right (BType (EnumType e), lambda, delta, omega)
checkTLit cls enums lambda delta omega _ = Nothing

checkTFldRef :: ExprCheck
checkTFldRef cls enums lambda delta omega e@(ExprReference (RefField name)) = 
    Just $ checkTFldRef' cls enums lambda delta omega e
checkTFldRef cls enums lambda delta omega _ = Nothing

checkTFldRef' :: ExprCheckInternal
checkTFldRef' cls enums lambda delta omega (ExprReference (RefField name)) = do
    let lstEl = lastDelta delta
    (o, s) <- fromMaybe (Left "Wrong stack size in TFldRef") $ Right <$> lstEl
    t <- ("could not find varaible " ++ name) ~~ lookupLambda lambda o name
    if lin t then
        Right (t, (updateLambda lambda o name BotType), delta, omega)
    else 
        Right (t, lambda, delta, omega) 

checkTParRef :: ExprCheck
checkTParRef cls enums lambda delta omega e@(ExprReference (RefParameter name)) = 
    Just $ checkTParRef' cls enums lambda delta omega e
checkTParRef cls enums lambda delta omega _ = Nothing

checkTParRef' :: ExprCheckInternal
checkTParRef' cls enums lambda delta omega e@(ExprReference (RefParameter name)) = do
    let lstEl = lastDelta delta
    (o, (x, t)) <- fromMaybe (Left "Wrong stack size in TParRef") $ Right <$> lstEl
    if lin t then
        Right (t, lambda, (initDelta delta) `with` (o, (x, BotType)), omega)
    else
        Right (t, lambda, delta, omega)


checkTCallF :: ExprCheck
checkTCallF cls enums lambda delta omega e@(ExprCall (RefField name) mthd exp) =
    Just $ checkTCallF' cls enums lambda delta omega e
checkTCallF cls enums lambda delta omega _ = Nothing

checkTCallF' :: ExprCheckInternal
checkTCallF' cls enums lambda delta omega (ExprCall (RefField f) m e) = do
    -- TODO: Figure out if we should include multiple transitions here
    (t, lambda', delta', omega') <- maybeEitherToEither "Could not typecheck parameter expression" $ 
                                    checkExpression cls enums lambda delta omega e
    let lstEl = lastDelta delta
    let lstEl' = lastDelta delta'
    (o, s) <- fromMaybe (Left "Wrong stack size in TCallF") $ Right <$> lstEl
    (o', s') <- fromMaybe (Left "Wrong stack size in TCallF") $ Right <$> lstEl'
    assert' (o == o') "Object names does not match in TCallF" $ do
    ftype <- fromMaybe (Left "Could not find field in TCallF") $ Right <$> lookupLambda lambda' o f
    case ftype of 
        (CType (c, gen, usage)) -> do
            let resultingUsages =  filterUsages m $ transitions usage
            assert' (length resultingUsages > 0) ("No transitions available for method call " ++ m)  $ do
            let w =  head resultingUsages
            let (Method tret _ ptype _ _) = getMethod cls c m
            assert' (t == ptype) ("Wrong parameter type in TCallF " ++ show t ++ " expected " ++ show ptype)$ do
            Right $ (tret, (updateLambda lambda' o f (CType (c, gen, w))), delta', omega')
        _ -> Left "Invalid type for field"

checkTCallP :: ExprCheck
checkTCallP cls enums lambda delta omega e@(ExprCall (RefParameter name) mthd exp) =
    Just $ checkTCallP' cls enums lambda delta omega e
checkTCallP cls enums lambda delta omega _ = Nothing

checkTCallP' :: ExprCheckInternal
checkTCallP' cls enums lambda delta omega (ExprCall (RefParameter x) m e) = do
    -- TODO: Figure out if we should include multiple transitions here
    (t, lambda', delta', omega') <- maybeEitherToEither "Could not typecheck parameter expression" $ checkExpression cls enums lambda delta omega e
    let lstEl = lastDelta delta
    let lstEl' = lastDelta delta'
    (o, s) <- fromMaybe (Left "Wrong stack size in TCallP") $ Right <$> lstEl
    (o', (x', t')) <- fromMaybe (Left "Wrong stack size in TCallP") $ Right <$> lstEl'
    assert' (o == o') "Object names does not match in TCallP" $ do
    assert' (x == x') "Parameter names does not match in TCallP" $ do
    case t' of 
        (CType (c, gen, usage)) -> do
            let resultingUsages = filterUsages m $ transitions usage
            assert' (length resultingUsages > 0) "No transitions available for method call" $ do
            let w =  head resultingUsages
            let (Method tret _ ptype _ _) = getMethod cls c m
            assert' (t == ptype) "Wrong parameter type in TCallP" $ do
            Right $ (tret, lambda', (initDelta delta') `with` (o', (x', (CType (c, gen, w)))), omega')
        _ -> Left "Invalid type for field"

lookupLabels :: Type -> [EnumDef] -> Maybe [String]
lookupLabels (BType (EnumType name)) def = lookupLabels' name def
lookupLabels _                       def = Nothing

lookupLabels' :: String -> [EnumDef] -> Maybe [String] 
lookupLabels' name ((EnumDef enumName lbls):xs) = if name == enumName 
                                                        then Just lbls 
                                                        else lookupLabels' name xs
lookupLabels' name [] = Nothing

availableChoices :: Usage -> Either String [String]
availableChoices usage = availableChoices' (current usage) (recursiveUsages usage) []
    where availableChoices' :: UsageImpl -> [(String, UsageImpl)] -> [String] -> Either String [String]
          availableChoices' (UsageChoice l)   _   _        = Right $ map fst l
          availableChoices' (UsageBranch l)   _   _        = Left "Branch usage is not a choice"
          availableChoices' (UsageEnd)        _   _        = Left "End usage is not a choice"
          availableChoices' (UsageVariable r) rec lookedAt = 
                if r `elem` lookedAt
                    then Left "infinite transition found"
                    else ("unable to find recursive variable" ~~ envLookup rec r) >>= \usage ->
                         availableChoices' usage rec (r:lookedAt)

findUsage :: Type -> Maybe Usage
findUsage (CType (name, gen, usage)) = Just usage 
findUsage _                     = Nothing

doTransition :: String -> Usage -> Either String Usage
doTransition name u@(Usage cur rec) = 
    case cur of
        (UsageChoice xs)  -> ("unable to find transition" ~~ envLookup xs name) >>= \cur' -> 
                             Right $ u{current = cur'}
        (UsageBranch xs)  -> "unable to find transition" ~~ envLookup xs name >>= \cur' ->
                             Right $ u{current = cur'}
        (UsageEnd)        -> Left $ "branch usage have no transitions"
        (UsageVariable r) -> "unable to find recursive variable" ~~ envLookup rec r >>= \cur' ->
                             doTransition name u{current = cur'}

checkTSwP :: ExprCheck
checkTSwP cls enums lambda delta omega e@(ExprSwitch (RefParameter _) _ _) = 
    Just $ checkTSwP' cls enums lambda delta omega e
checkTSwP cls enums lambda delta omega _ = Nothing

checkTSwP' :: ExprCheckInternal
checkTSwP' cls enums lambda delta omega expr@(ExprSwitch (RefParameter x) e _) = do
    (t, lambda'', delta'', omega'') <- maybeEitherToEither "Could not typecheck parameter expression" $ checkExpression cls enums lambda delta omega e
    let lstEl = lastDelta delta
    let lstEl' = lastDelta delta''
    (o, s) <- fromMaybe (Left "Wrong stack size in TSwP") $ Right <$> lstEl
    (o', (x', t')) <- fromMaybe (Left "Wrong stack size in TSwP") $ Right <$> lstEl'
    assert' (o == o') "Objects does not match in TSwP" $ do
    assert' (x == x') "Parameter does not match in checkTSwP" $ do
    lbls <- ("failed to lookup enum" ~~  lookupLabels t enums)
    usage <- "must be a classtype" ~~ findUsage t'
    transitions <- availableChoices usage
    assert' (sort lbls == sort transitions) "label and transitions do not match" $ do
    assert' (length lbls > 0) "no labels found" $ do
    -- todo check all transitions end up in the same state 
    let afterTransition = map (checkTSwP'' cls enums lambda'' delta'' omega'' expr usage) transitions
    let (failed, succeeded) = partitionEithers afterTransition    
    assert' (null failed) "some transitions failed" $ do
    let compareable = head succeeded 
    assert' (all (== compareable) succeeded) "not all transitions lead to same values" $ do
    compareable

checkTSwP'' cls enums lambda delta omega expr usage transition = do
    expr' <- switchExpr' expr     
    usage' <- doTransition transition usage
    let lastEl = lastDelta delta 
    (o, (x, t)) <- maybeToEither "Left wrong stack size" lastEl
    case t of 
        (CType (c, gen, _)) ->  
            let delta' = initDelta delta `with` (o, (x, (CType (c, gen, usage'))))
                result = "check expression failed" ~~ checkExpression cls enums lambda delta' omega expr'
            in result
        _              -> Left "failed to lookup type"
    where switchExpr' (ExprSwitch _ _ choices) = 
                "could not find transition in switch checkTSwP" ~~ envLookup choices transition
          switchExpr' _                        = 
                Left $ "could not find transition in switch checkTSwP"
     

checkTSwF :: ExprCheck
checkTSwF cls enums lambda delta omega e@(ExprSwitch (RefField _) _ _) = 
    Just $ checkTSwF' cls enums lambda delta omega e
checkTSwF cls enums lambda delta omega _ = Nothing

checkTSwF' :: ExprCheckInternal
checkTSwF' cls enums lambda delta omega expr@(ExprSwitch (RefField f) e _) = do
    (t, lambda'', delta'', omega'') <- maybeEitherToEither "Could not typecheck parameter expression" $ checkExpression cls enums lambda delta omega e
    let lstEl = lastDelta delta
    let lstEl' = lastDelta delta''
    (o, s) <- fromMaybe (Left "Wrong stack size in TSwP") $ Right <$> lstEl
    (o', s') <- fromMaybe (Left "Wrong stack size in TSwP") $ Right <$> lstEl'
    assert' (o == o') "Objects does not match in TSwP" $ do
    ftype <- maybeToEither "could not find fild in CheckTSwF" $ lookupLambda lambda'' o f
    lbls <- "failed to lookup enum" ~~  lookupLabels t enums
    usage <- "must be a classtype" ~~ findUsage ftype
    transitions <- availableChoices usage
    assert' (sort lbls == sort transitions) "label and transitions do not match" $ do
    assert' (length lbls > 0) "no labels found" $ do
    -- todo check all transitions end up in the same state 
    let afterTransition = map (checkTSwF'' cls enums lambda'' delta'' omega'' expr usage f) transitions
    let (failed, succeeded) = partitionEithers afterTransition    
    assert' (null failed) ("some transitions failed " ++ show failed) $ do
    let compareable = head succeeded 
    assert' (all (== compareable) succeeded) "not all transitions lead to same values" $ do
    compareable

checkTSwF'' cls enums lambda delta omega expr usage f transition = do
    expr' <- switchExpr' expr     
    usage' <- doTransition transition usage
    let lastEl = lastDelta delta 
    (o, _) <- maybeToEither "Left wrong stack size" lastEl
    t <- "failed to find field" ~~ lookupLambda lambda o f
    case t of 
        (CType (c, gen, _)) -> let lambda' = updateLambda lambda o f (CType (c, gen, usage'))
                          in "check expression failed" ~~ checkExpression cls enums lambda' delta omega expr'
        _              -> Left $ "failed to lookup type " ++ show t 
    where switchExpr' (ExprSwitch _ _ choices) = 
                "could not find transition in switch checkTSwF" ~~ envLookup choices transition
          switchExpr' _                        = 
                Left $ "could not find transition in switch checkTSwF"
 

checkTProg :: [Class] -> [EnumDef] -> Either String ()
checkTProg cls enums = 
    checkTProg' cls enums cls

checkTProg' cls enums [] = Right ()
checkTProg' cls enums (c:cs) = 
    let checkTClassRes = checkTClass cls enums c
        checkTProgRes = checkTProg' cls enums cs
        res = checkTClassRes >> checkTProgRes
    in res

checkTClass :: [Class] -> [EnumDef] -> Class -> Either String ()
checkTClass cls enums c = 
    let term = checkTUsage cls enums [] (initFields (cfields c)) c (cusage c) 
        term' = "checkTUSage failed" `maybeEitherToEither` term
    in  term' >>= \term'' -> 
            if isJust term'' && terminatedEnv (fromJust term'')
                then Right () 
                else Left "Invalid terminal env"

initFields :: [Field] -> [(FieldName, Type)]
initFields [] = []
initFields ((Field (BaseFieldType b) name):flds) = (name, BType b):(initFields flds)
initFields ((Field (ClassFieldGen c gen) name):flds) = (name, BotType):(initFields flds)
initFields ((Field GenericField name):flds) = error "Wrong field type, not instantiated"

terminatedEnv :: FieldTypeEnv -> Bool
terminatedEnv [] = True
terminatedEnv ((n, t):env) = not (lin t) && terminatedEnv env

checkTUsage :: UsageCheck
checkTUsage cls enums theta envTf c usage = 
    checkTCBr  cls enums theta envTf c usage
    <|> checkTCCh  cls enums theta envTf c usage
    <|> checkTCEn  cls enums theta envTf c usage
    <|> checkTCVar cls enums theta envTf c usage
    <|> checkTCRec cls enums theta envTf c usage
    

allEqual :: [FieldTypeEnv] -> Bool
allEqual []        = True
allEqual (a:[])    = True
allEqual (a:b:lst) = a === b && allEqual (b:lst)
    
findMethod :: [Method] -> String -> Method
findMethod ms m = 
     head $ filter (\ml -> (mname ml) == m) ms

checkTCBr :: UsageCheck
checkTCBr cls enums theta envTf c (Usage (UsageBranch lst) bindings) = 
    let bres = map (\(s,i) -> checkTCBr' cls enums theta envTf c s (s, i, bindings)) lst
        errors = lefts bres
        envs = rights bres
        envsToCheck = catMaybes envs 
    in case True of
        True | not (null errors)    -> Just . Left $ unlines errors
             | (null envsToCheck)   -> Just $ Right $ Nothing
             | allEqual envsToCheck -> Just $ Right $ Just $ head envsToCheck
             | otherwise            -> Just $ Left "Environments do not match TCBr"
checkTCBr cls enums theta envTf c _ = Nothing

terminated = not . lin

checkTCBr' :: [Class] -> [EnumDef] -> RecursiveEnv -> FieldTypeEnv -> Class -> String -> (String, UsageImpl, [(String, UsageImpl)]) -> Either String (Maybe FieldTypeEnv)
checkTCBr' cls enums theta envTf c mname (label, uimpl, bindings) = do
    let method = findMethod (cmethods c) mname
        lambda = [("this", ((cname c, CType (cname c, GenericBot, Usage uimpl bindings)), envTf))]
        delta = ([], [("this", (parname method, partype method))]) 
        theta = []
    (ti', lambda', delta', theta') <- "check expression failed in tcbr" `maybeEitherToEither` checkExpression cls enums lambda delta [] (mexpr method)
    let lstEl = lastDelta delta'
    (o, (pname, ti'')) <- fromMaybe (Left "Wrong stack size in TCBr") $ Right <$> lstEl
    (_, envTf'') <- fromMaybe (Left "Bla") $ Right <$> envLookup lambda' "this"
    --assert' (ti'' == (partype method)) "Wrong resulting parameter type TCBr" $ do
    assert' (terminated ti'') "Parameter must be terminated TCBr" $ do
    let res = fromJust $ checkTUsage cls enums theta envTf'' c (Usage uimpl bindings)
    res

checkTCCh :: UsageCheck
checkTCCh cls enums theta envTf c (Usage (UsageChoice lst) bindings) = 
    let finals' = map (\(label, usage) -> checkTUsage cls enums theta envTf c (Usage usage bindings)) lst
        finals =  catMaybes $ finals'
        actualEnvs = catMaybes $ rights finals  
    in Just $ case True of 
        True -- | length finals /= length actualEnvs -> Left "Could not typecheck usage in TCCh" 
             | null actualEnvs                    -> Right Nothing
             | allEqual actualEnvs                -> Right . Just $ (head actualEnvs) 
             | otherwise                          -> Left "Final envs do not match TTCh"
checkTCCh cls enums theta envTf c _ = Nothing

checkTCEn :: UsageCheck
checkTCEn cls enums theta envTf c (Usage UsageEnd _) = 
    Just $ Right $ Just envTf
checkTCEn cls enums theta envTf c _ = Nothing


checkTCVar :: UsageCheck
checkTCVar cls enums theta envTf c (Usage (UsageVariable x) us)  = do
    if isNothing $ envLookup us x
        then Just $ Right $ Nothing
        else Nothing
checkTCVar cls enums theta envTf c _ = Nothing

checkTCRec :: UsageCheck
checkTCRec cls enums theta envTf c (Usage (UsageVariable x) us) = do
    u <- us `envLookup` x
    checkTUsage cls enums ((x, envTf) : theta) envTf c (Usage u (us `without` x))
checkTCRec cls enums theta envTf c _ = Nothing
