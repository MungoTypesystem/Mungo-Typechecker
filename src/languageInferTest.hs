module LanguageInferTest where

import MungoParser 
import AstTransformer
import AST 
import Data.Either
import Control.Monad
import SanityCheck
import qualified TypeSystemTest as TST
import qualified TypeSystem as TS
import UsageInference
import Data.Graph
import Data.List
import Data.Graph.Visualize
import Data.Graph.DGraph
import Data.Graph.Connectivity
import Data.Graph.Types
import Debug.Trace
import Data.Maybe
import Data.Ord

simpleFile = 
    "../ExamplePrograms/nestedinfer.mg"

graphPng = 
    "../ExamplePrograms/Output.png"


run = runFile simpleFile

runFile :: String -> IO ()
runFile s = do
    file <- readFile s 
    let parsed = parseProgram file
    either putStrLn checkCST parsed

checkCST :: CstProgram -> IO ()
checkCST prog = do
    let check = [] :: [String] -- sanityCheck [prog] -- [] :: [String]
    if not $ null check
        then forM_ check putStrLn >> putStrLn ("error in sanity " ++ show check)
        else convertCST prog

convertCST :: CstProgram -> IO ()
convertCST prog = do
    let converted = convertProgram prog
    either putStrLn inferCheck' converted 

inferCheck :: ([Class], [EnumDef]) -> IO ()
inferCheck (classes, enums) = do
    let classes' = map maybeInfer $ fromJust (sortAcyclic classes)
    forM_ classes' (putStrLn . show . cusage)
    typeCheck (classes', enums)
    where maybeInfer :: Class -> Class
          maybeInfer cls =
                if cusage cls == UsageInference 
                    then let u = inferUsage cls classes enums
                         in trace (cname cls ++ "---" ++ show u) $ cls {cusage = u} 
                    else cls

inferCheck' :: ([Class], [EnumDef]) -> IO ()
inferCheck' (classes, enums) = do
    forM_ classes maybeInfer 
    where maybeInfer :: Class -> IO () 
          maybeInfer cls =
                if cusage cls == UsageInference 
                    then let (g, _) = inferGraph cls classes enums 
                         in void $ plotDGraphPng g graphPng
                    else return ()

typeCheck :: ([Class], [EnumDef]) -> IO ()
typeCheck (classes, enums) = do 
    let typeCheckOld = TS.checkTProg classes enums 
    let typeCheckNew = TST.checkTProg classes enums 
    putStrLn $ "old " ++ show typeCheckOld
    putStrLn $ "new " ++ show typeCheckNew
    return ()


sortAcyclic :: [Class] -> Maybe [Class]
sortAcyclic cls = do
    if isAcyclic then Just (sortedList ++ simple) else Nothing
    where 
        (toInfer, simple) = partition shouldInfer cls 
        shouldInfer clazz = (cusage clazz) == UsageInference
        arcList = [((-->) :: String->String->Arc String ())(cname c') (cname c) | c <- toInfer, c' <- toInfer, c `hasFieldOf` c']
        graph = fromArcsList arcList
        sortedList = sortBy isConnectedInGraph toInfer
        isConnectedInGraph c c' = if areConnected graph (cname c) (cname c') then LT else GT
        hasFieldOf c c' = 
            any fieldTypeIsC' fields
            where fields = cfields c
                  fieldTypeIsC' f = case ftype f of 
                    (ClassFieldGen cn _) -> cname c' == cn
                    _ -> False
        isAcyclic = any (\(v1,v2) -> areConnected graph v1 v2 && areConnected graph v2 v1) [(v1, v2) | v1 <- (map cname toInfer), v2 <- (map cname toInfer)]