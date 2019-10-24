module Main where

import Lib
import MungoParser
import TypeSystem
import SanityCheck
import AST
import Data.Either
import Control.Monad
import Control.Applicative
import LanguageTest
import Data.Graph
import LanguageTest
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    runFile $ head args

sequence' :: [a] -> [IO a] -> IO [a]
sequence' l []     = return l
sequence' l (x:xs) = do 
    x' <- x
    sequence' (x':l) xs

testFile1 = "../Mungo-Inference/ExamplePrograms/SimpleExample.mg"
testFile2 = "../Mungo-Inference/ExamplePrograms/UseAnotherClassExample.mg"
testFile3 = "../Mungo-Inference/ExamplePrograms/generic.mg"

readFiles = sequence' [] $ map (parseProgram <$>) $ map readFile [testFile3]

check = do
    program <- readFiles
    let ll = lefts program
    let rr = rights program
    --viewGraph rr
    if not (null ll) 
        then forM_ ll putStrLn
        else checkSanity $ rr

checkSanity :: [CstProgram] -> IO ()
checkSanity program = do
    let errs = sanityCheck program
    forM_ errs putStrLn
    return ()

testParseSwitch = parseProgram <$> readFile "C:/Users/mikkel/Documents/GitHub/Mungo-Inference/ExamplePrograms/SwitchExample.mg"
