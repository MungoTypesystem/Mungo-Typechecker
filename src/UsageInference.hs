module UsageInference where

import StateMonadError
import AST
import Data.Either
import Data.Maybe
import Control.Arrow
import Control.Monad (when)
import Data.List ((\\), union, nub, groupBy)
import Data.Graph hiding (vertices)
import Data.Graph.DGraph
import Data.Graph.Types hiding (union)
import Data.Graph.Connectivity
import Data.Graph.Traversal
import TypeSystem

import Debug.Trace


inferenceClass :: Class -> Lambda -> [Class] -> [EnumDef]-> [(Lambda, String, [Lambda])]
inferenceClass cls ftypeenv clazzes enumz =
    ftypeenv'
    where
        ftypeenv'' = map evaluateInference m
        ftypeenv' = rights ftypeenv''

        evaluateInference :: (Lambda, String, Expression, String, Type) -> Either String (Lambda, String, [Lambda])
        evaluateInference (envtf, m, expr, x, t) =
            let envDelta = Delta [] [("this", (x, t))]
                envOmega = Omega []
                newenvs  = Environments envtf envDelta envOmega
                classData = ClassData clazzes (cname cls)
                myState   = MyState newenvs classData enumz
                infered   = runState (checkExpression expr) [(myState, BotType)]
            in case infered of 
                    Right (_, v) ->Right (envtf, m, map (lambda . environments. fst) v)
                    Left err     -> Left err

        m :: [(Lambda, String, Expression, String, Type)]
        m = map (\methd -> (ftypeenv, mname methd, mexpr methd, parname methd,partype methd)) $ cmethods cls

        rec = []

inferUsage' :: Class -> [Class] -> [EnumDef] -> [Lambda] -> [(Lambda, String, [Lambda])]
inferUsage' cls clazzes enumz toSearch =
    concat $ map inferClassUsage toSearch
    where inferClassUsage env = 
                inferenceClass cls env clazzes enumz

inferUsage'' :: Class -> [Class] -> [EnumDef] -> [(Lambda, String, [Lambda])] -> [Lambda] -> [Lambda] -> [(Lambda, String, [Lambda])]
inferUsage'' cls clazzes enumz res seen []          = res
inferUsage'' cls clazzes enumz res seen (x:notSeen) = 
    let result        = inferUsage' cls clazzes enumz [x]
        res'          = result ++ res
        seen'         = if x `elem` seen then seen else x:seen
        possibleFound = concat $ map (\(_, _, env) -> env) result
        notSeen'      = notSeen `union` possibleFound \\ seen'
    in inferUsage'' cls clazzes enumz res' seen' notSeen'
    

inferUsage''' :: Class -> [Class] -> [EnumDef] -> [(Lambda, String, [Lambda])]
inferUsage''' cls clazzes enumz = 
    inferUsage'' cls clazzes enumz [] [] [initialLambda cls]

inferUsage :: Class -> [Class] -> [EnumDef] -> Usage
inferUsage cls clazzes enumz =
    graphToUsage cls enumz $ inferGraph cls clazzes enumz

inferGraph :: Class -> [Class] -> [EnumDef] -> (DGraph Int [String], Int)
inferGraph cls clazzes enums = 
    (finalGraph, firstVertex)
    where 
          finalGraph = insertEdgeTriples [(v, endVertex, m) | v <- reachStart 
                                                            , firstVertex' <- firstVerticies
                                                            , let Just (_, _, m) = edgeTriple intermediateGraph v firstVertex'] intermediateGraph :: DGraph Int [String]
          reachStart = [v | v <- finalVertices
                          , firstVertex' <- firstVerticies
                          , containsEdgePair intermediateGraph (v, firstVertex')]
          endVertex = 0
          intermediateGraph = removeVertices unreachableVertices largeGraph
          unreachableVertices = [v | v <- vertices largeGraph, not (v `elem` finalVertices)]
          finalVertices = [ v | firstVertex' <- firstVerticies
                              , v <- reachableVertices, areConnected largeGraph v firstVertex' ]
          reachableVertices = bfsVertices largeGraph firstVertex
          largeGraph =  insertEdgeTriples l' empty
          l = inferUsage''' cls clazzes enums
          envs = nub $ [ env' | (env, m, listEnv) <- l, env' <- listEnv ] ++
                       [ env | (env, _, _) <- l]
          firstVertex = find (initialLambda cls)
          firstVerticies = map find $ filter isTerminatedLambda $ map fst envs' :: [Int]
          envs' :: [(Lambda, Int)]
          envs' = zip envs [1..]
          find :: Lambda -> Int
          find k = fromJust (k `lookup` envs')
          l' = map combineLists l'''
          combineLists :: [(Int, Int, String)] -> (Int, Int, [String])
          combineLists ls =
                let (from, to) = (\(a, b, _) -> (a, b)) $ head ls
                in (from, to, map (\(_, _, m) -> m) ls)

          l''' = groupBy (\(a, b, _) (a', b', _) -> a == a' && b == b') l''
          l'' = [ (find env, find env', m)
                | (env, m, listEnv) <- l
                , env' <- listEnv
                ]

initialLambda :: Class -> Lambda
initialLambda cls = l
    where 
          f = initFields (cfields cls)
          l = [("this", ((cname cls, CType (cname cls, GenericBot, UsageInference)), f))]

graphToUsage :: Class -> [EnumDef] -> (DGraph Int [String], Int) -> Usage 
graphToUsage cls enums (g, first) = 
    if null g'
        then Usage UsageEnd recu
        else Usage (UsageVariable ("X" ++ show first)) recu
    where g'' = toList g
          g' = [(from, rest) | (from, ls) <- g'' 
                              , let rest = [(to, m) | (to, mls) <- ls
                                                    , m <- mls ] ]
          isChoice m = 
                let mthds = cmethods cls 
                    mthd  = head $ filter ((m ==) . mname) mthds
                    mthdRet = rettype mthd
                in case mthdRet of
                        (BType (EnumType t)) -> let (EnumDef _ enumTypes) = 
                                                       head $ filter (\(EnumDef n _) -> n == t) enums 
                                                in Just enumTypes
                        _                    -> Nothing 

          convertList ls = if null ls 
                                then UsageEnd 
                                else convertBranches ls

          recu = map (\(v, ls) -> ("X" ++ show v, convertList ls)) g'

          convertBranches' (v', x) = 
                let target = if v' == 0 
                                then UsageEnd 
                                else UsageVariable ("X" ++ (show v')) 
                in case isChoice x of 
                            Just t  -> let choices = map (\m -> (m, target)) t
                                       in [(x, UsageChoice choices), (x, target)]
                            Nothing -> [(x, target)]

          convertBranches ls = 
                UsageBranch $ concat $ map convertBranches' ls

isTerminatedLambda :: Lambda -> Bool
isTerminatedLambda l = 
    let env = head l
    in terminatedEnv $ snd . snd $ env
