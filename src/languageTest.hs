import MungoParser 
import AstTransformer
import AST 
import Data.Either
import Control.Monad
import SanityCheck

simpleFile = 
    "C:/Users/mikkel/Documents/GitHub/Mungo-Inference/ExamplePrograms/SimpleExample2.mg"


runFile :: String -> IO ()
runFile s = do
    file <- readFile s 
    let parsed = parseProgram file
    either putStrLn checkCST parsed

checkCST :: CstProgram -> IO ()
checkCST prog = do
    let check = sanityCheck [prog]
    if not $ null check
        then forM_ check putStrLn
        else convertCST prog

convertCST :: CstProgram -> IO ()
convertCST prog = do
    putStrLn . show $ prog
    let converted = convertProgram prog
    either putStrLn printAST converted


printAST :: ([Class], [EnumDef]) -> IO ()
printAST (classes, enums) = do
    putStrLn "enums:"
    forM_ enums (putStrLn . show)
    putStrLn "classes:"
    forM_ classes (putStrLn . show)

