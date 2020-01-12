module LanguageInferTest where

import MungoParser 
import AstTransformer
import AST 
import Data.Either
import Control.Monad
import SanityCheck
import qualified TypeSystem as TS
import qualified UsageInference as UI
import Data.Graph
import Data.List
import Data.Graph.Visualize
import Data.Graph.DGraph
import Data.Graph.Connectivity
import Data.Graph.Types
import Data.Maybe
import Data.Ord
import Debug.Trace

simpleFile = 
    "../ExamplePrograms/BoolAndInteger.mg"

graphPng = 
    "../ExamplePrograms/Output.png"


run = runFile simpleFile

runFile :: String -> IO ()
runFile s = do
    file <- readFile s 
    let parsed = parseProgram file
    putStrLn $ show parsed
    either putStrLn checkCST parsed

checkCST :: CstProgram -> IO ()
checkCST prog = do
    let check =  sanityCheck [prog]  -- [] :: [String]
    if not $ null check
        then forM_ check putStrLn >> putStrLn ("error in sanity " ++ show check)
        else convertCST prog

convertCST :: CstProgram -> IO ()
convertCST prog = do
    let converted = convertProgram prog
    either putStrLn inferCheck converted 

inferCheck :: ([Class], [EnumDef]) -> IO ()
inferCheck (classes, enums) = do
    let classes' = inferFold [] $ fromJust (sortAcyclic classes)
    forM_ classes' (putStrLn . show . cusage)
    typeCheck (classes', enums)
    where 
          inferFold done []            = done
          inferFold done (cls:classes) =
                let cls' = maybeInfer cls done
                in inferFold (cls':done) classes

          maybeInfer :: Class -> [Class] -> Class
          maybeInfer cls clazzes =
                if cusage cls == UsageInference 
                    then let u = UI.inferUsage cls (cls:clazzes) enums
                         in cls {cusage = u} 
                    else cls

typeCheck :: ([Class], [EnumDef]) -> IO ()
typeCheck (classes, enums) = do 
    let typeCheck = TS.checkTProg classes enums 
    --putStrLn $ "old " ++ show typeCheckOld
    putStrLn $ show typeCheck
    return ()


sortAcyclic :: [Class] -> Maybe [Class]
sortAcyclic [] = Just []
sortAcyclic cls = do
    if isAcyclic || null sortedList then Just (simple ++ sortedList) else Nothing
    where 
        (toInfer, simple) =  partition shouldInfer cls
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
