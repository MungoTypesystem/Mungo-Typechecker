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
import Data.Graph.Visualize
import Debug.Trace

simpleFile = 
    "../ExamplePrograms/Infer1.mg"

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
    either putStrLn inferCheck converted 

inferCheck :: ([Class], [EnumDef]) -> IO ()
inferCheck (classes, enums) = do
    let classes' = map maybeInfer classes
    forM_ classes' (putStrLn . show . cusage)
    typeCheck (classes', enums)
    where maybeInfer :: Class -> Class
          maybeInfer cls =
                if cusage cls == UsageInference 
                    then let u = inferUsage cls classes enums
                         in trace (cname cls ++ "---" ++ show u) $ cls {cusage = u} 
                    else cls

typeCheck :: ([Class], [EnumDef]) -> IO ()
typeCheck (classes, enums) = do 
    let typeCheckOld = TS.checkTProg classes enums 
    let typeCheckNew = TST.checkTProg classes enums 
    putStrLn $ "old " ++ show typeCheckOld
    putStrLn $ "new " ++ show typeCheckNew
    return ()
