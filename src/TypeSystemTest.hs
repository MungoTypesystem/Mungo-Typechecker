module TypeSystemTest where

import AST
import Data.Maybe
import Control.Monad (ap, liftM, guard, liftM2, forM)
import Data.Maybe
import Debug.Trace

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
                                 } deriving (Show)

data ClassData = ClassData { allClasses    :: [Class]
                           , checkingClass :: String 
                           }
    deriving (Show)

data MyState = MyState { environments :: Environments
                       , classData    :: ClassData 
                       } deriving (Show)

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
lastM (x:[]) = return x
lastM (_:xs) = lastM xs
lastM []     = fail "" 

fromMaybeM :: Monad m => Maybe a -> m a
fromMaybeM (Just x) = return x
fromMaybeM Nothing  = fail "nothing"

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

lastDelta :: Delta -> DTypeSystem (ObjectName, (ParameterName, Type)) 
lastDelta delta = lastM $ dParameterStackTypeEnv delta

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

getLambdaField :: String -> DTypeSystem Type 
getLambdaField fieldname = do
    classname <- getCurrentClass
    lambda <- getLambda
    ((clazzname, t), env)<- classname `envLookupIn` lambda
    fieldname `envLookupIn` env

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


checkExpression :: Expression -> NDTypeSystem () 
checkExpression (ExprNew classname gen)     = checkTNew classname gen
checkExpression (ExprAssign fieldname expr) = checkTFld fieldname expr
checkExpression (ExprCall ref mname e)      = checkTCall ref mname e
checkExpression (ExprSeq e1 e2)             = checkTSeq e1 e2

checkTNew :: String -> GenericInstance -> NDTypeSystem () 
checkTNew cn gen = forAll $ do
    currentState <- getMyState 
    cls <- findClass cn
    let u = cusage cls
    let t = CType (cn, gen, u)
    return $ [(currentState, t)]

checkTFld :: String -> Expression -> NDTypeSystem () 
checkTFld fieldname e = forAll $ do
    (lambda, delta)     <- getEnvironments 
    (oname, (x, ftype)) <- lastDelta delta
    ((cname, _), env)   <- oname `envLookupIn` lambda
    let ndChecked = checkExpression e
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
        updateLambda oname fieldname t'
        state' <- getMyState
        return $ [(state', BType VoidType)]

checkTCall :: Reference -> MethodName -> Expression -> NDTypeSystem () 
checkTCall (RefField name)     mname expr = checkTCallF name mname expr
checkTCall (RefParameter name) mname expr = checkTCallP name mname expr

checkTCallF :: String -> MethodName -> Expression -> NDTypeSystem () 
checkTCallF fieldName mname expr = forAll $ do
    delta <- getDelta
    (o, _) <- lastDelta delta
    let ndChecked = checkExpression e
    convertNDToD $ forAllIn ndChecked $ do
        delta' <- getDelta
        lambda' <- getLambda
        (o', _) <- lastDelta delta'
        assert' (o' == o)
        ftype <- o `envLookupIn` lambda'
        case ftype of
            (CType (c, gen, usage)) -> return ()
            _                       -> fail "Invalid type for field"
        let (CType (c, gen, usage)) = ftype
        let resultingUsages = filterUsage m $ transitions usage 
        (Method tret _ ptype _ _) <- getMethod m gen
        assert' (t == ptype)
        return $ do
            w <- resultingUsages
            return (state >> updateLambda o fieldname (CType (c, gen, w)),
                    tret)


checkTCallP :: String -> MethodName -> Expression -> NDTypeSystem () 
checkTCallP parametername mname expr = forAll $ do
    fail "not implemented"

checkTSeq :: Expression -> Expression -> NDTypeSystem () 
checkTSeq e1 e2 = do
    t <- checkExpression e1
    -- check that everything is done
    checkExpression e2
    fail "abc"

emptyEnv = Environments l d o
    where l = [] 
          d = Delta [] []
          o = Omega []

emptyState = MyState emptyEnv (ClassData [Class "cls" ClassNoGeneric (Usage UsageEnd []) [] []] "")
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
