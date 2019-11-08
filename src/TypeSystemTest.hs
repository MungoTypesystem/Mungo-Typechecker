module TypeSystemTest where

import AST
import Data.Maybe
import Control.Monad (ap, liftM, guard, liftM2, forM, when)
import Debug.Trace
import Control.Arrow (second)
import Data.List (nub, sort)

data MState s a = MState { runState :: s -> Maybe (a, s)}
                | MStateError 

instance Show (MState s a) where
    show (MState _)   = "mstate"
    show (MStateError) = "mstate fail"

instance Functor (MState s) where
    fmap = Control.Monad.liftM

instance Applicative (MState s) where
    pure  = return
    (<*>) = Control.Monad.ap

instance Monad (MState s) where
    return x         = MState $ \s -> Just (x, s)
    fail str         = MStateError 
    (MState h) >>= f = MState $ \s -> do (a, newState) <- h s 
                                         case f a of
                                            (MState g) -> g newState
                                            otherwise  -> Nothing
    (MStateError) >>= f = MStateError                                    

getState :: MState s s
getState = MState $ \m -> Just (m, m)

setState :: s -> MState s ()
setState v = MState $ \m -> Just ((), v)

{--evalState :: MState s a -> s -> a
evalState act = fst . runState act --}
 
execState :: MState s a -> s -> s -> s
execState act otherwise startState = fromMaybe otherwise (snd <$> runState act startState)

type FieldTypeEnv = [(FieldName, Type)] 

type Lambda = [(ObjectName, ((ClassName, Type), FieldTypeEnv))]

-- EnvTO
type ObjectTypeEnv = [(ObjectName, Typestate)]

-- EnvT, EqS
type ParameterStackTypeEnv = [(ObjectName, (ParameterName, Type))]


data Delta  = Delta { dObjectTypeEnv         :: ObjectTypeEnv 
                    , dParameterStackTypeEnv :: ParameterStackTypeEnv
                    }
    deriving (Show, Eq)

data Omega  = Omega [(String, (Lambda, Delta))] 
    deriving (Show, Eq)

data Errors = NoErrors
            | Errors [String]
    deriving (Show)

data Environments = Environments { lambda :: Lambda 
                                 , delta  :: Delta 
                                 , omega :: Omega
                                 } deriving (Show, Eq)

data ClassData = ClassData { allClasses    :: [Class]
                           , checkingClass :: String 
                           }
    deriving (Show)

data MyState = MyState { environments :: Environments
                       , classData    :: ClassData 
                       , enumsData    :: [EnumDef]
                       } deriving (Show)


instance Eq MyState where
    m1 == m2 = environments m1 == environments m2

type NDTypeSystem a = MState [(MyState, Type)] a
type DTypeSystem  a = MState (MyState, Type) a

rewriteStates :: [(MyState, Type)] -> NDTypeSystem ()
rewriteStates ls = MState $ \_ -> Just ((), ls)

getMyState ::  DTypeSystem MyState
getMyState = MState $ \m -> Just (fst m, m)

getReturnType :: DTypeSystem Type 
getReturnType = MState $ \m -> Just (snd m, m)

forAll :: DTypeSystem [(MyState, Type)] -> NDTypeSystem ()
forAll f = do
    states <- getState
    let states' = concat $ map fst $ catMaybes $ map (runState f) states 
    --let envs    = map snd' states' 
    --let values  = map fst' states' 
    rewriteStates states' 
    -- make sure all values are the same
    return ()
forAllIn :: NDTypeSystem () -> DTypeSystem [(MyState, Type)] -> NDTypeSystem ()
forAllIn ts f = ts >> forAll f


headM :: Monad m => [a] -> m a
headM (x:_) = return x
headM []    = fail ""

lastM :: Monad m => [a] -> m a
lastM [] = fail "" 
lastM xs = return $ last xs

initM :: Monad m => [a] -> m [a]
initM [] = fail ""
initM xs = return $ init xs


fromMaybeM :: Monad m => Maybe a -> m a
fromMaybeM (Just x) = return x
fromMaybeM Nothing  = fail "nothing"

-- helper rewrite classes
insertTopTypeClassEasy = 
    insertTopTypeClass (ClassFieldGen "$generic" GenericBot) (CType ("$generic", GenericBot, UsageTop))


insertTopTypeClass :: FieldType -> Type -> Class -> Class 
insertTopTypeClass ft tt c = 
    let f = map (insertTopTypeField ft) (cfields c)
        m = map (insertTopTypeMethod tt) (cmethods c)
    in  c {cfields = f, cmethods = m}

insertTopTypeField ::  FieldType -> Field ->Field 
insertTopTypeField ft f = 
    let newFt = insertTopTypeFieldType ft $ ftype f
    in f{ftype = newFt}
        
insertTopTypeFieldType :: FieldType -> FieldType -> FieldType
insertTopTypeFieldType replacement (GenericField) = replacement -- ClassFieldGen "$generic" GenericBot 
insertTopTypeFieldType _           a              = a 

insertTopTypeMethod :: Type -> Method -> Method
insertTopTypeMethod t m =
    let rt = insertTopTypeType t $ rettype m
        pt = insertTopTypeType t $ partype m
    in m {rettype = rt, partype = pt}

insertTopTypeType :: Type -> Type -> Type
insertTopTypeType replacement GType = replacement -- CType ("$generic", GenericBot, UsageTop) 
insertTopTypeType replacement a     = a




-- state lookup
envApply :: (Environments -> a) -> DTypeSystem a
envApply f = MState $ \m -> Just $ (f (environments (fst m)), m)

getClassData :: DTypeSystem ClassData
getClassData = MState $ \m -> Just (classData (fst m), m)

findClass :: String -> DTypeSystem Class
findClass name = do
    classes <- getAllClasses
    let found = filter ((name ==) . cname) classes
    headM found

getCurrentClass :: DTypeSystem String
getCurrentClass = checkingClass <$> getClassData

getAllClasses :: DTypeSystem [Class]
getAllClasses = allClasses <$> getClassData

getDelta :: DTypeSystem Delta 
getDelta = envApply delta
    
getLambda :: DTypeSystem Lambda 
getLambda = envApply lambda 

getOmega :: DTypeSystem Omega 
getOmega = envApply omega 

getEnvironments :: DTypeSystem (Lambda, Delta)
getEnvironments = liftM2 (,) getLambda getDelta

getEnums :: DTypeSystem [EnumDef]
getEnums = MState $ \m -> Just (enumsData (fst m), m)

lastDelta :: Delta -> DTypeSystem (ObjectName, (ParameterName, Type)) 
lastDelta delta = lastM $ dParameterStackTypeEnv delta

setDelta :: Delta -> DTypeSystem ()
setDelta d = do
    (myState, t) <- getState
    let env = environments myState
    let env' = env {delta = d} 
    let myState' = myState {environments = env'}
    setState (myState', t)
    

convertNDToD :: NDTypeSystem () -> DTypeSystem [(MyState, Type)]
convertNDToD nd = do 
    s <- getState
    (a, newStates) <- fromMaybeM $ runState nd [s]
    return $ newStates

envLookupIn :: (Monad m, Eq a) => a -> [(a, b)] -> m b
envLookupIn k l = fromMaybeM (k `lookup` l)

getField :: String -> DTypeSystem FieldType
getField fieldname = do
    classname <- getCurrentClass
    cls       <- getAllClasses
    clazz <- headM $ filter (\c -> cname c == classname) cls 
    field <- headM $ filter (\f -> fname f == fieldname) $ cfields clazz
    return $ ftype field

getMethod methodname GenericBot = do 
    classname <- getCurrentClass
    cls       <- getAllClasses
    clazz <- headM $ filter (\c -> cname c == classname) cls
    headM $ filter (\m -> mname m == methodname) $ cmethods clazz
    
getMethod methodname (GenericInstance gcname g u) = do
    classname <- getCurrentClass
    cls       <- getAllClasses
    clazz <- headM $ filter (\c -> cname c == classname) cls
    let t'    = CType (gcname, g, u)
    m <- headM $ filter (\m -> mname m == methodname) $ cmethods clazz
    return $ insertTopTypeMethod t' m

getLambdaField :: String -> DTypeSystem Type 
getLambdaField fieldname = do
    classname <- getCurrentClass
    lambda <- getLambda
    ((clazzname, t), env)<- classname `envLookupIn` lambda
    fieldname `envLookupIn` env

fileTypeCType t@(CType (c, gen, usage)) = return t
fileTypeCType _                         = fail "did not work"

without :: [(String, a)] -> String -> [(String, a)]
without env name = filter ((/= name) . fst) env

replaceFieldType :: FieldTypeEnv -> String -> Type -> FieldTypeEnv
replaceFieldType env name ntype = map (\(n, t) -> if n == name then (n, ntype) else (n, t)) env

updateLambda :: Lambda -> ObjectName -> FieldName -> Type -> Lambda 
updateLambda lambda oname field ntype = 
    let (c, env) = fromJust $ oname `lookup` lambda 
        newEnv = replaceFieldType env field ntype
    in 
        (oname, (c, newEnv)):(without lambda oname)

updateLambdaM :: ObjectName -> String -> Type -> DTypeSystem () 
updateLambdaM oname field ntype = do
    l <- getLambda
    let l' = updateLambda l oname field ntype
    (myState, tret) <- getState 
    let env = environments myState
    let env' = env { lambda = l'}
    setState $ (myState {environments = env'}, tret)

updateOmegaM :: Omega -> DTypeSystem ()
updateOmegaM o = do
    (myState, tret) <- getState
    let env = environments myState
    let env' = env {omega = o}
    setState $ (myState {environments = env'}, tret)

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
   fromMaybe [] $ transitions' recU <$> (str `envLookupIn` recU)

filterUsages trans lst = map snd $ filter (\(l, u) -> l == trans) lst

lin :: Type -> Bool
lin (CType (cname, _, UsageTop)) = True
lin (CType (cname, _, usage))    = current usage /= UsageEnd
lin _ = False

agree :: FieldType -> Type -> Bool
agree (BaseFieldType b1) (BType b2)                    = b1 == b2
agree (ClassFieldGen cn1 gen1) (CType (cn2, gen2, _) ) = cn1 == cn2 && gen1 == gen2
agree (ClassFieldGen cn1 gen) (BotType)                = True
agree _ _ = False

assert' :: Monad m => Bool -> m ()
assert' True  = return ()
assert' False = fail "" 

initDelta :: Delta -> Delta
initDelta (Delta x y) = Delta x (init y)

addDelta :: Delta -> (ObjectName, (ParameterName, Type)) -> Delta
addDelta (Delta x y) el = Delta x (y ++ [el])

with = addDelta 

findEnum :: [EnumDef] -> LabelName -> DTypeSystem String
findEnum [] litteral = fail "" 
findEnum ((EnumDef name litterals):es) litteral = 
    if (any (== litteral) litterals) 
            then return name 
            else findEnum es litteral

cTypeM :: Monad m => Type -> m Type
cTypeM t@(CType _) = return t
cTypeM _           = fail ""

lookupLabels :: Type -> DTypeSystem [String]
lookupLabels (BType (EnumType name)) = do
    def <- getEnums
    lookupLabels' name def
lookupLabels _                       = fail "" 


lookupLabels' :: Monad m => String -> [EnumDef] -> m [String] 
lookupLabels' name ((EnumDef enumName lbls):xs) = if name == enumName 
                                                        then return lbls 
                                                        else lookupLabels' name xs
lookupLabels' name [] = fail "" 

findUsage :: Monad m => Type -> m Usage
findUsage (CType (name, gen, usage)) = return usage 
findUsage _                          = fail "" 

availableChoices :: Monad m => Usage -> m [String]
availableChoices usage = availableChoices' (current usage) (recursiveUsages usage) []
    where availableChoices' :: Monad m => UsageImpl -> [(String, UsageImpl)] -> [String] -> m [String]
          availableChoices' (UsageChoice l)   _   _        = return $ map fst l
          availableChoices' (UsageBranch l)   _   _        = fail "Branch usage is not a choice"
          availableChoices' (UsageEnd)        _   _        = fail "End usage is not a choice"
          availableChoices' (UsageVariable r) rec lookedAt = 
                if r `elem` lookedAt
                    then fail "infinite transition found"
                    else (r `envLookupIn` rec) >>= \usage ->
                         availableChoices' usage rec (r:lookedAt)


doTransition :: Monad m => String -> Usage -> m Usage
doTransition name u@(Usage cur rec) = 
    case cur of
        (UsageChoice xs)  -> (name `envLookupIn` xs) >>= \cur' -> 
                             return $ u{current = cur'}
        (UsageBranch xs)  -> name `envLookupIn` xs >>= \cur' ->
                             return $ u{current = cur'}
        (UsageEnd)        -> fail $ "branch usage have no transitions"
        (UsageVariable r) -> r `envLookupIn` rec >>= \cur' ->
                             doTransition name u{current = cur'}

validCalculations :: [(MyState, Type)] -> [[(MyState, Type)]] -> [(MyState, Type)]
validCalculations (x:xs) ls = if all (x `elem`) ls
                                    then x : validCalculations xs ls
                                    else validCalculations xs ls
validCalculations []     ls = [] 


checkExpression :: Expression -> NDTypeSystem () 
checkExpression (ExprNew classname gen)     = checkTNew classname gen
checkExpression (ExprAssign fieldname expr) = checkTFld fieldname expr
checkExpression (ExprCall ref mname e)      = checkTCall ref mname e
checkExpression (ExprSeq e1 e2)             = checkTSeq e1 e2
checkExpression (ExprIf e1 e2 e3)           = checkTIf e1 e2 e3
checkExpression (ExprLabel lbl e1)          = checkTLab lbl e1
checkExpression (ExprContinue lbl)          = checkTCon lbl
checkExpression (ExprBoolConst b)           = checkTBool b
checkExpression (ExprNull)                  = checkTBot
checkExpression (ExprUnit)                  = checkTUnit 
checkExpression (ExprSwitch ref e cases)    = checkTSwitch ref e cases
checkExpression (ExprReturn e)              = checkTRet e
checkExpression (ExprReference ref)         = checkTRef ref
checkExpression (ExprLitteral str)          = checkTLit str
checkExpression (ExprObjectName o)          = checkTObj o 

checkTNew :: String -> GenericInstance -> NDTypeSystem () 
checkTNew cn gen = forAll $ do
    currentState <- getMyState 
    cls <- findClass cn
    let u = cusage cls
    let t = CType (cn, gen, u)
    return $ [(currentState, t)]

checkTFld :: String -> Expression -> NDTypeSystem () 
checkTFld fieldname expr = forAll $ do
    (lambda, delta)     <- getEnvironments 
    (oname, (x, ftype)) <- lastDelta delta
    ((cname, _), env)   <- oname `envLookupIn` lambda
    let ndChecked = checkExpression expr
    convertNDToD $ forAllIn ndChecked $ do
        t' <- getReturnType
        f <- getField fieldname
        assert' (agree f t')
        t <- getLambdaField fieldname 
        assert' (not (lin t))
        (lambda', delta') <- getEnvironments
        ((cname, typelookup), envlookup) <- oname `envLookupIn` lambda'
        (oname', s') <- lastDelta delta'
        assert' (oname == oname')
        updateLambdaM oname fieldname t'
        state' <- getMyState
        return $ [(state', BType VoidType)]

checkTCall :: Reference -> MethodName -> Expression -> NDTypeSystem () 
checkTCall (RefField name)     mname expr = checkTCallF name mname expr
checkTCall (RefParameter name) mname expr = checkTCallP name mname expr

checkTCallF :: String -> MethodName -> Expression -> NDTypeSystem () 
checkTCallF fieldName mname expr = forAll $ do
    delta <- getDelta
    (o, _) <- lastDelta delta
    let ndChecked = checkExpression expr
    convertNDToD $ forAllIn ndChecked $ do
        t <- getReturnType
        delta' <- getDelta
        lambda' <- getLambda
        (o', _) <- lastDelta delta'
        assert' (o' == o)
        ftype <- getLambdaField fieldName --o `envLookupIn` lambda'
        (CType (c, gen, usage)) <- fileTypeCType ftype 
        let resultingUsages = filterUsages mname $ transitions usage
        (Method tret _ ptype _ _) <- getMethod mname gen
        assert' (t == ptype)
        (currentState, _) <- getState
        let currentEnvironment = environments currentState 
        return $ do
            w <- resultingUsages
            let lambda''  = lambda currentEnvironment
            let lambda''' = updateLambda lambda'' o fieldName (CType (c, gen, w))
            let currentEnvironment' = currentEnvironment { lambda = lambda'' } 
            return (currentState {environments = currentEnvironment' }, 
                    tret)


checkTCallP :: String -> MethodName -> Expression -> NDTypeSystem () 
checkTCallP parametername mname expr = forAll $ do
    delta <- getDelta
    (o, _) <- lastDelta delta
    let ndChecked = checkExpression expr
    convertNDToD $ forAllIn ndChecked $ do
        t <- getReturnType
        delta' <- getDelta
        (o', (x', t')) <- lastDelta delta'
        assert' (o == o')
        assert' (parametername == x')
        (CType (c, gen, usage)) <- cTypeM t
        let resultingUsages = filterUsages mname $ transitions usage
        assert' (length resultingUsages > 0)
        (Method tret _ ptype _ _) <- getMethod mname gen
        assert' (t == ptype)
        (lastState, _) <- getState
        return $ do
            w <- resultingUsages
            let newPType = CType (c, gen, w)
            let finalDelta = initDelta delta' `with` (o', (x', newPType))
            let lastEnv = environments lastState
            let finalEnv = lastEnv { delta = finalDelta } 
            let finalState = lastState { environments = finalEnv  }
            return (finalState, tret)
        
        

checkTSeq :: Expression -> Expression -> NDTypeSystem () 
checkTSeq e1 e2 = do
    checkExpression e1
    forAll $ do
        s <- getState
        t <- getReturnType
        assert' (lin t)
        return [s]
    checkExpression e2

checkTIf :: Expression -> Expression -> Expression -> NDTypeSystem () 
checkTIf e1 e2 e3 = do
    checkExpression e1
    forAll $ do    
        s <- getState
        t <- getReturnType
        assert' (t == BType BoolType)
        return [s]
    forAll $ do
        s <- getState
        -- make sure they are all equals
        (_, e2Res) <- fromMaybeM $ runState (checkExpression e2) [s]
        (_, e3Res) <- fromMaybeM $ runState (checkExpression e3) [s]
        
        let e2Res' = [res | res <- e2Res, res `elem` e3Res]
        let e3Res' = [res | res <- e3Res, res `elem` e2Res]
        
        assert' (not (null e2Res')) 
        assert' (not (null e3Res')) 

        let res = nub $ e2Res' ++ e3Res'
        
        return res

checkTLab :: String -> Expression -> NDTypeSystem ()
checkTLab lbl e1 = forAll $ do
    (Omega lbls) <- getOmega
    let found = lbl `envLookupIn` lbls
    assert' (isNothing found)
    (lambda, delta) <- getEnvironments    
    let lbls' = (lbl, (lambda, delta)) : lbls
    updateOmegaM (Omega lbls')
    s <- getState
    (_, states') <- fromMaybeM $ runState (checkExpression e1) [s]
    return states'

checkTCon :: String -> NDTypeSystem ()
checkTCon lbl = forAll $ do
    (Omega lbls)      <- getOmega
    (lambda, delta)   <- getEnvironments 
    (lambda', delta') <- fromMaybeM $ lbl `envLookupIn` lbls
    assert' (lambda == lambda')
    assert' (delta == delta')
    s <- getState
    return [s]
    
checkTBool :: Bool -> NDTypeSystem ()
checkTBool b = forAll $ do
    (s, _) <- getState
    return [(s, BType BoolType)]

checkTBot :: NDTypeSystem ()
checkTBot = forAll $ do
    (s, _) <- getState
    return [(s, BotType)]

checkTUnit :: NDTypeSystem ()
checkTUnit = forAll $ do
    (s, _) <- getState
    return [(s, BType VoidType)]

checkTSwitch :: Reference -> Expression -> [(String, Expression)] -> NDTypeSystem ()
checkTSwitch (RefField fieldname)         e cases = checkTSwF fieldname e cases
checkTSwitch (RefParameter parametername) e cases = checkTSwP parametername e cases

checkTSwP :: String -> Expression -> [(String, Expression)] -> NDTypeSystem ()
checkTSwP x expr cases = forAll $ do
    delta <- getDelta
    (o, s) <- lastDelta delta
    let ndChecked = checkExpression expr
    convertNDToD $ forAllIn ndChecked $ do
        t <- getReturnType
        delta'' <- getDelta
        (o', (x', t')) <- lastDelta delta
        assert' (o == o')
        assert' (x == x')
        lbls <- lookupLabels t
        usage <- findUsage t'
        transitions <- availableChoices usage
        assert' (sort lbls == sort transitions)
        assert' (not (null lbls))
        s <- getState
        let computations = map (checkTSwP' cases usage) transitions
        let computations' = map (\c -> runState c s) computations
        let computations'' = filter isJust computations' 
        assert' (length computations'' == length computations')
        let computations''' = map (fst . fromJust) computations'' 
        computationsHead <- headM computations''' -- :: [(MyState, Type)]
        let computationsTail = tail computations'''
        let accepting = validCalculations computationsHead computationsTail  
        return accepting
    
checkTSwP' :: [(String, Expression)] -> Usage -> String -> DTypeSystem [(MyState, Type)]
checkTSwP' expr usage transition = do
    usage' <- doTransition transition usage
    delta <- getDelta
    (o, (x, t)) <- lastDelta delta
    (CType (c, gen, _)) <- cTypeM t
    let delta' = initDelta delta `with` (o, (x, (CType (c, gen, usage'))))
    setDelta delta'
    expr' <- transition `envLookupIn` expr
    let ndChecked = checkExpression expr'
    convertNDToD ndChecked 

checkTSwF :: String -> Expression -> [(String, Expression)] -> NDTypeSystem ()
checkTSwF f expr cases = forAll $ do
    delta <- getDelta
    let ndChecked = checkExpression expr
    convertNDToD $ forAllIn ndChecked $ do
        t <- getReturnType
        delta' <- getDelta
        (o, s) <- lastDelta delta
        (o', s') <- lastDelta delta'
        assert' (o == o')
        ftype <- getLambdaField f
        lbls <- lookupLabels t 
        usage <- findUsage ftype
        transitions <- availableChoices usage
        assert' (sort lbls == sort transitions)
        assert' (length lbls > 0)
        s <- getState
        let computations = map (checkTSwF' cases usage f) transitions
        let computations' = map (\c -> runState c s) computations
        let computations'' = filter isJust computations' 
        assert' (length computations'' == length computations')
        let computations''' = map (fst . fromJust) computations'' 
        computationsHead <- headM computations''' -- :: [(MyState, Type)]
        let computationsTail = tail computations'''
        let accepting = validCalculations computationsHead computationsTail  
        return accepting


checkTSwF' :: [(String, Expression)] -> Usage -> String -> String -> DTypeSystem [(MyState, Type)]
checkTSwF' expr usage f transition = do
    delta <- getDelta
    usage' <- doTransition transition usage
    (o, _) <- lastDelta delta
    t <- getLambdaField f
    (CType (c, gen, _)) <- cTypeM t
    updateLambdaM o f (CType (c, gen, usage'))
    expr' <- transition `envLookupIn` expr
    let ndChecked = checkExpression expr'
    convertNDToD ndChecked 

checkTRet :: Expression -> NDTypeSystem ()
checkTRet e = forAll $ do
    delta3 <- getDelta
    (o, s) <- lastDelta delta3
    let delta = initDelta delta3
    setDelta delta
    let ndChecked = checkExpression e
    convertNDToD $ forAllIn ndChecked $ do
        delta4 <- getDelta
        let delta3 = initDelta delta4 
        setDelta (delta3 `with` (o,s))
        s <- getState
        return [s]

checkTRef :: Reference -> NDTypeSystem ()
checkTRef (RefParameter parametername) = checkTParRef parametername
checkTRef (RefField fieldname)         = checkTFldRef fieldname

checkTParRef :: String -> NDTypeSystem ()
checkTParRef parametername = forAll $ do
    delta <- getDelta
    (o, (x, t)) <- lastDelta delta
    (myState, _) <- getState
    when (lin t) $ setDelta ((initDelta delta) `with` (o, (x, BotType)))
    updatedState <- getState
    return [updatedState]
    
checkTFldRef :: String -> NDTypeSystem ()
checkTFldRef fieldname = forAll $ do 
    delta <- getDelta
    (o, s) <- lastDelta delta
    t <- getLambdaField fieldname
    when (lin t) $ updateLambdaM o fieldname BotType
    s <- getState
    return [s]
    
checkTLit :: String -> NDTypeSystem ()
checkTLit lit = forAll $ do
    enums <- getEnums
    e <- findEnum enums lit
    (s, _) <- getState
    return [(s, BType (EnumType e))]
    
checkTObj :: String -> NDTypeSystem ()
checkTObj o = forAll $ do 
    delta <- getDelta
    let envTo = dObjectTypeEnv delta
    typestate <- o `envLookupIn` envTo
    let envTo' = envTo `without` o
    let delta' = delta {dObjectTypeEnv = envTo'}
    setDelta delta'
    (s, _) <- getState
    return [(s, (CType typestate))]


emptyEnv = Environments l d o
    where l = [] 
          d = Delta [] []
          o = Omega []

emptyState = MyState emptyEnv (ClassData [Class "cls" ClassNoGeneric (Usage UsageEnd []) [] []] "") []
e = (ExprNew "cls" GenericBot)
m = runState (checkExpression e) [(emptyState, BotType)]

{--type NDState a = MState [Int] a
type DState a  = MState Int a

rewriteStates :: [Int] -> NDState ()
rewriteStates ls = MState $ \_ -> Just ((), ls)

forAll :: DState [(a, Int)] -> NDState [a]
forAll f = do
    states <- getState
    let states' = concat $ map fst $ catMaybes $ map (runState f) states 
    let envs    = map snd states' :: [Int]
    let values  = map fst states' 
    rewriteStates envs
    return values 

checkExpr :: Expr -> NDState [Int]
checkExpr ExprAdd         = exprAdd
checkExpr (ExprEq n)      = exprEq n
checkExpr (ExprSeq e1 e2) = exprSeq e1 e2

exprAdd :: NDState [Int]
exprAdd = forAll $ do
    v <- getState
    return $ do 
        m <- [0,2]
        return (v, (v + m))

exprEq :: Int -> NDState [Int]
exprEq n = forAll $ do
    v <- getState
    --trace ("|" ++ show v ++ show n ++ "|") $ assert' (v == n)
    return $ return (v, v)

exprSeq :: Expr -> Expr -> NDState [Int]
exprSeq e1 e2 = do
    checkExpr e1
    checkExpr e2

assert' :: Monad m => Bool -> m ()
assert' True  = fail "failed"
assert' False = return ()

e = (ExprSeq (ExprSeq ExprAdd ExprAdd) (ExprEq 2))
v = runState (checkExpr e) [0] --}
