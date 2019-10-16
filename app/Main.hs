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

main :: IO ()
main = putStrLn "hello world"

sequence' :: [a] -> [IO a] -> IO [a]
sequence' l []     = return l
sequence' l (x:xs) = do 
    x' <- x
    sequence' (x':l) xs

testFile1 = "../Mungo-Inference/ExamplePrograms/SimpleExample.mg"
testFile2 = "../Mungo-Inference/ExamplePrograms/UseAnotherClassExample.mg"

readFiles = sequence' [] $ map (parseProgram <$>) $ map readFile [testFile1]

check = do
    program <- readFiles
    --return program
    let ll = lefts program
    let rr = rights program

    if not (null ll) 
        then forM_ ll putStrLn
        else checkSanity $ rr

checkSanity :: [CstProgram] -> IO ()
checkSanity program = do
    let errs = sanityCheck program
    forM_ errs putStrLn
    return ()

testParseSwitch = parseProgram <$> readFile "C:/Users/mikkel/Documents/GitHub/Mungo-Inference/ExamplePrograms/SwitchExample.mg"
