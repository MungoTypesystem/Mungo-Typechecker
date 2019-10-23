module Sanitychecktest where

import MungoParser
import SanityCheck
import Data.Either
import Control.Monad
import Control.Applicative
import Data.Graph

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