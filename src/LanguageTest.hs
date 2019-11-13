module LanguageTest where

import MungoParser 
import AstTransformer
import AST 
import Data.Either
import Control.Monad
--import SanityCheck
import TypeSystemTest

simpleFile = 
    "../ExamplePrograms/generic.mg"


runFile :: String -> IO ()
runFile s = do
    file <- readFile s 
    let parsed = parseProgram file
    either putStrLn checkCST parsed
    --either putStrLn checkCST parsed

checkCST :: CstProgram -> IO ()
checkCST prog = do
    let check = [] :: [String] --sanityCheck [prog] 
    if not $ null check
        then forM_ check putStrLn >> putStrLn ("error in sanity " ++ show check)
        else convertCST prog

convertCST :: CstProgram -> IO ()
convertCST prog = do
    let converted = convertProgram prog
    --either putStrLn (putStrLn . show) converted
    either putStrLn typeCheck converted --}

typeCheck :: ([Class], [EnumDef]) -> IO ()
typeCheck (classes, enums) = do 
    {--putStrLn "enums:"
    forM_ enums (putStrLn . show)--}
    putStrLn "classes:"
    forM_ classes (putStrLn . show)
    let typeCheck = checkTProg classes enums 
    putStrLn $ show typeCheck
    return ()
--}
