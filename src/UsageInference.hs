module UsageInference where

import StateMonadError
import AST
import Data.Either
import Data.Maybe
import Control.Arrow
import Control.Monad (when)
import Data.List ((\\), union, nub)
import Data.Graph hiding (vertices)
import Data.Graph.DGraph
import Data.Graph.Types hiding (union)
import Data.Graph.Connectivity
import Data.Graph.Traversal
import Debug.Trace

type FieldTypeEnv = [(String, Type)]


type RecursiveEnv = [(String, FieldTypeEnv)]

data InferenceState = InferenceState { fieldTypeEnv :: FieldTypeEnv
                                     , recursiveEnv :: RecursiveEnv
                                     , classes :: [Class]
                                     , enums :: [EnumDef]
                                     }

instance Eq InferenceState where
    inference1 == inference2 = fieldTypeEnv inference1 == fieldTypeEnv inference2

type NDUsageInference a = MState [(InferenceState, Type)] a
type DUsageInference a = MState (InferenceState, Type) a

forAll :: DUsageInference [(InferenceState, Type)] -> NDUsageInference ()
forAll f = do
    states <- getState
    let states' = concat $ map fst $ rights $ map (runState f) states 
    setState states' 
    return ()

findClass :: String -> DUsageInference Class
findClass name = do
    (s, _) <- getState
    let clazzes = classes s 
    let found = filter ((name ==) . cname) clazzes
    headM found

getFieldTypeEnv :: DUsageInference FieldTypeEnv
getFieldTypeEnv = fieldTypeEnv . fst <$> getState

updateField :: FieldTypeEnv -> String -> Type -> FieldTypeEnv
updateField env fieldname t = 
    map updateEnv env
    where updateEnv (fname, t') = 
                if fname == fieldname
                        then (fname, t)
                        else (fname, t')

updateFieldM :: String -> Type -> DUsageInference ()
updateFieldM fieldname t = do
    env <- getFieldTypeEnv    
    let env' = updateField env fieldname t
    (s, oldtype) <- getState
    let s' = s {fieldTypeEnv = env'}
    setState (s', oldtype)

getMethod classname methodname = do 
    (s, _) <- getState
    let cls = classes s 
    clazz <- headM $ filter (\c -> cname c == classname) cls
    headM $ filter (\m -> mname m == methodname) $ cmethods clazz
 
cTypeM :: Monad m => Type -> m Type
cTypeM t@(CType _) = return t
cTypeM _           = fail ""

lin :: Type -> Bool
lin (CType (cname, _, UsageTop)) = True
lin (CType (cname, _, usage))    = current usage /= UsageEnd
lin _ = False

transitions :: Usage -> [(String, Usage)]
transitions u = map toUsage $ transitions' (recursiveUsages u) (current u) 
    where recursives = recursiveUsages u
          toUsage :: (String, UsageImpl) -> (String, Usage)
          toUsage = second ((flip Usage) recursives)

findEnum :: [EnumDef] -> LabelName -> DUsageInference String
findEnum [] litteral = fail "" 
findEnum ((EnumDef name litterals):es) litteral = 
    if (any (== litteral) litterals) 
            then return name 
            else findEnum es litteral

availableChoices :: Monad m => Usage -> m [String]
availableChoices usage = availableChoices' (current usage) (recursiveUsages usage) []
    where availableChoices' :: Monad m => UsageImpl -> [(String, UsageImpl)] -> [String] -> m [String]
          availableChoices' (UsageChoice l)   _   _        = return $ map fst l
          availableChoices' (UsageBranch l)   _   _        = fail "Branch usage is not a choice"
          availableChoices' (UsageEnd)        _   _        = fail "End usage is not a choice"
          availableChoices' (UsageVariable r) rec lookedAt = 
                if r `elem` lookedAt
                    then fail "infinite transition found"
                    else (fromMaybeM (r `lookup` rec)) >>= \usage ->
                         availableChoices' usage rec (r:lookedAt)


transitions' :: [(String, UsageImpl)] -> UsageImpl ->  [(String, UsageImpl)]
transitions' recU UsageEnd             = []
transitions' recU (UsageBranch lst)    = lst
transitions' recU (UsageChoice lst)    = lst
transitions' recU (UsageVariable str) = 
   fromMaybe [] $ transitions' recU <$> (str `lookup` recU)

filterUsages trans lst = map snd $ filter (\(l, u) -> l == trans) lst

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

convertNDToD :: NDUsageInference () -> DUsageInference [(InferenceState, Type)]
convertNDToD nd = do 
    s <- getState
    (a, newStates) <- fromEitherM $ runState nd [s]
    return $ newStates

validCalculations :: [(InferenceState, Type)] -> [[(InferenceState, Type)]] -> [(InferenceState, Type)]
validCalculations (x:xs) ls = if all (x `elem`) ls
                                    then x : validCalculations xs ls
                                    else validCalculations xs ls
validCalculations []     ls = [] 


  
inference :: Expression -> NDUsageInference ()
inference (ExprNew c _)      = inferenceNew c 
inference (ExprAssign f e)   = inferenceFld f e 
inference (ExprCall r m e)   = inferenceCall r m e
inference (ExprSeq e1 e2)    = inferenceSeq e1 e2
inference (ExprIf c e1 e2)   = inferenceIf c e1 e2
inference (ExprLabel l e)    = inferenceLbl l e
inference (ExprContinue l)   = inferenceCon l
inference (ExprBoolConst b)  = inferenceBool b
inference (ExprNull)         = inferenceNull
inference (ExprUnit)         = inferenceUnit
inference (ExprSwitch r e c) = inferenceSwitch r e c
inference (ExprReference r)  = inferenceRef r
inference (ExprLitteral s)   = inferenceLit s

inferenceNew :: String -> NDUsageInference ()
inferenceNew classname = forAll $ do
    c <- findClass classname
    let usage = cusage c
    (s, _) <- getState
    return [(s, CType (classname, GenericBot, usage))]

inferenceFld :: String -> Expression -> NDUsageInference ()
inferenceFld fieldname expression = do
    inference expression
    forAll $ do
        fieldTypeE <- getFieldTypeEnv
        ftype <- fromMaybeM $ fieldname `lookup` fieldTypeE
        assert' $ not (lin ftype)
        (_, t) <- getState
        updateFieldM fieldname t 
        (s, _) <- getState
        return [(s, BType VoidType)]

inferenceCall :: Reference -> String -> Expression -> NDUsageInference ()
inferenceCall (RefParameter _) _ _ = fail "parameter cannot be inferenced"
inferenceCall (RefField f) m e     = inferenceCallF f m e

inferenceCallF :: String -> String -> Expression -> NDUsageInference ()
inferenceCallF fieldname m e = forAll $ do
    fieldTypeE <- getFieldTypeEnv
    ftype <- fromMaybeM $ fieldname `lookup` fieldTypeE
    (CType (cn, g, usage)) <- cTypeM ftype
    let resultingUsages = filterUsages m $ transitions usage
    (Method tret _ ptype _ _) <- getMethod cn m
    (s, _) <- getState
    let env = fieldTypeEnv s
    return $ do
        w <- resultingUsages
        let env' = updateField env fieldname (CType (cn, g, w))
        let s' = s { fieldTypeEnv = env' }
        return (s', tret)

inferenceSeq :: Expression -> Expression -> NDUsageInference ()
inferenceSeq e1 e2 = do
    inference e1
    forAll $ do
        (s, t) <- getState
        assert' $ not (lin t)
        return [(s, t)]
    inference e2

inferenceIf :: Expression -> Expression -> Expression -> NDUsageInference ()
inferenceIf c e1 e2 = do
    inference c
    forAll $ do
        (s, t) <- getState
        assert' (t == BType BoolType)
        return [(s, t)]
    forAll $ do
        s <- getState
        (_, e1Res) <- fromEitherM $ runState (inference e1) [s]
        (_, e2Res) <- fromEitherM $ runState (inference e2) [s]
        let res = [res | res <- e1Res, res `elem` e2Res]
        assert' (not (null res)) 
        return res

inferenceLbl :: String -> Expression -> NDUsageInference ()
inferenceLbl lbl e = forAll $ do
    (s, t) <- getState
    let rec = recursiveEnv s
    assert' $ isNothing (lbl `lookup` rec)
    let rec' = (lbl, fieldTypeEnv s) : rec 
    let s' = s { recursiveEnv = rec' }
    (_, states') <- fromEitherM $ runState (inference e) [(s', t)]
    return states'

inferenceCon :: String -> NDUsageInference ()
inferenceCon lbl = forAll $ do
    (s, t) <- getState
    let rec = recursiveEnv s
    fenv <- fromMaybeM (lbl `lookup` rec)
    assert' $ fenv == fieldTypeEnv s
    return [(s, BType VoidType)]

inferenceBool :: Bool -> NDUsageInference ()
inferenceBool b = forAll $ do
    (s, _) <- getState
    return [(s, BType BoolType)]

inferenceNull :: NDUsageInference ()
inferenceNull = forAll $ do
    (s, _) <- getState
    return [(s, BotType)]

inferenceUnit :: NDUsageInference ()
inferenceUnit = forAll $ do
    (s, _) <- getState
    return [(s, BType VoidType)]

inferenceSwitch :: Reference -> Expression -> [(String, Expression)] -> NDUsageInference ()
inferenceSwitch (RefParameter _) _ _ = fail "cannot infer SwF"
inferenceSwitch (RefField f) e cases = inferenceSwF f e cases

inferenceSwF :: String -> Expression -> [(String, Expression)] -> NDUsageInference ()
inferenceSwF f e cases = do
    inference e
    forAll $ do
        fieldTypeE <- getFieldTypeEnv
        ftype <- fromMaybeM $ f `lookup` fieldTypeE
        (CType (_, _, usage)) <- cTypeM ftype
        transitions <- availableChoices usage
        s <- getState
        let computations = map (inferenceSwF' cases usage f) transitions
        let computations' = map (\c -> runState c s) computations
        let computations'' = filter isRight computations' 
        assert' (length computations'' == length computations')
        let computations''' = map fst $ rights computations'' 
        computationHead <- headM computations'''
        let computationTail = tail computations'''
        let accepting = validCalculations computationHead computationTail 
        return accepting


inferenceSwF' :: [(String, Expression)] -> Usage -> String -> String -> DUsageInference [(InferenceState, Type)]
inferenceSwF' expr usage f transition = do
    usage' <- doTransition transition usage
    fieldTypeE <- getFieldTypeEnv
    ftype <- fromMaybeM $ f `lookup` fieldTypeE
    (CType (c, gen, _)) <- cTypeM ftype
    updateFieldM f (CType (c, gen, usage'))
    expr' <- transition `envLookupIn` expr
    convertNDToD $ inference expr'
    
inferenceRef :: Reference -> NDUsageInference ()
inferenceRef (RefParameter p) = fail "cannot infer parameter reference"
inferenceRef (RefField f)     = forAll $ do
    fieldTypeE <- getFieldTypeEnv
    ftype <- fromMaybeM $ f `lookup` fieldTypeE
    when (lin ftype) $ updateFieldM f BotType
    (s, _) <- getState
    return [(s, ftype)]

inferenceLit :: String -> NDUsageInference ()
inferenceLit lit = forAll $ do
    (s, _) <- getState
    let enumz = enums s 
    e <- findEnum enumz lit
    (s, _) <- getState
    return [(s, BType (EnumType e))]

inferenceClass :: Class -> FieldTypeEnv -> [Class] -> [EnumDef]-> [(FieldTypeEnv, String, [FieldTypeEnv])]
inferenceClass cls ftypeenv clazzes enumz =
    ftypeenv'
    where
        ftypeenv' = rights $ map evaluateInference m

        evaluateInference :: (FieldTypeEnv, String, Expression) -> Either String (FieldTypeEnv, String, [FieldTypeEnv])
        evaluateInference (env, m, expr) =
            let infered = runState (inference expr) [(s, BotType)]
            in case infered of 
                    Right v  -> Right (env, m, map (fieldTypeEnv . fst) (snd v))
                    Left err -> Left err

        m :: [(FieldTypeEnv, String, Expression)]
        m = map (\methd -> (ftypeenv, mname methd, mexpr methd)) $ cmethods cls

        s = InferenceState ftypeenv rec clazzes enumz

        rec = []

initFields :: [Class] -> [Field] -> FieldTypeEnv
initFields c = map (initField c)

initField :: [Class] -> Field -> (String, Type)
initField c f = (fname f, initFieldType c (ftype f))

initFieldType :: [Class] -> FieldType -> Type
initFieldType _       (BaseFieldType b)            = BType b
initFieldType clazzes (ClassFieldGen c GenericBot) = 
    let recu = recursiveUsages . cusage . head $ filter ((c ==) . cname) clazzes 
    in CType (c, GenericBot, Usage UsageEnd recu)

inferUsage' :: Class -> [Class] -> [EnumDef] -> [FieldTypeEnv] -> [(FieldTypeEnv, String, [FieldTypeEnv])]
inferUsage' cls clazzes enumz toSearch =
    concat $ map inferClassUsage toSearch
    where inferClassUsage env = 
                inferenceClass cls env clazzes enumz

inferUsage'' :: Class -> [Class] -> [EnumDef] -> [(FieldTypeEnv, String, [FieldTypeEnv])] -> [FieldTypeEnv] -> [FieldTypeEnv] -> [(FieldTypeEnv, String, [FieldTypeEnv])]
inferUsage'' cls clazzes enumz res seen []          =  res
inferUsage'' cls clazzes enumz res seen (x:notSeen) = 
    let result = inferUsage' cls clazzes enumz [x]
        res'   = result ++ res
        seen'  = if x `elem` seen then seen else x:seen
        possibleFound = concat $ map (\(_, _, env) -> env) result
        notSeen' = notSeen `union` possibleFound \\ seen'
    in inferUsage'' cls clazzes enumz res' seen' notSeen'
    

inferUsage''' :: Class -> [Class] -> [EnumDef] -> [(FieldTypeEnv, String, [FieldTypeEnv])]
inferUsage''' cls clazzes enumz = 
    inferUsage'' cls clazzes enumz [] [] [(initFields clazzes (cfields cls))]

inferUsage :: Class -> [Class] -> [EnumDef] -> Usage
inferUsage cls clazzes enumz =
    graphToUsage $ inferGraph cls clazzes enumz

inferGraph :: Class -> [Class] -> [EnumDef] -> (DGraph Int String, Int)
inferGraph cls clazzes enums = 
    (finalGraph, firstVertex)
    where 
          finalGraph = insertEdgeTriples [(v, endVertex, m) | v <- reachStart 
                                                            , let Just (_, _, m) = edgeTriple intermediateGraph v firstVertex] intermediateGraph :: DGraph Int String
          reachStart = [v | v <- finalVertices, containsEdgePair intermediateGraph (v, firstVertex)]--reachableAdjacentVertices' intermediateGraph firstVertex
          endVertex = 0
          intermediateGraph = removeVertices unreachableVertices largeGraph
          unreachableVertices = [v | v <- vertices largeGraph, not (v `elem` finalVertices)]
          finalVertices = [ v | v <- reachableVertices, areConnected largeGraph v firstVertex ]
          reachableVertices = bfsVertices largeGraph firstVertex
          largeGraph =  insertEdgeTriples l' empty
          l = inferUsage''' cls clazzes enums
          envs = nub $ [ env' | (env, m, listEnv) <- l, env' <- listEnv ] ++
                       [ env | (env, _, _) <- l]
          firstVertex = find (initFields clazzes (cfields cls))
          envs' :: [(FieldTypeEnv, Int)]
          envs' = zip envs [1..]
          find :: FieldTypeEnv -> Int
          find k = fromJust (k `lookup` envs')
          -- [(FieldTypeEnv, String, [FieldTypeEnv])] -> 
          -- [FieldTypeEnv FieldTypeEnv String]
          l' = [ (find env, find env', m)
               | (env, m, listEnv) <- l
               , env' <- listEnv
               ]

graphToUsage :: (DGraph Int String, Int) -> Usage 
graphToUsage (g, first) = 
    Usage (UsageVariable (show first)) recu
    where g' = toList g
          recu = map (\(v, ls) -> (show v, if null ls then UsageEnd else convertBranches ls)) g'
          convertBranches ls = UsageBranch $ map (\(v', x) -> (x, if v' == 0 then UsageEnd else UsageVariable (show v'))) ls

