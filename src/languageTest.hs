import MungoParser 
import AstTransformer
import AST 
import Data.Either
import Control.Monad
import SanityCheck
import TypeSystem

simpleFile = 
    "C:/Users/mikkel/Documents/GitHub/Mungo-Inference/ExamplePrograms/VerySimple.mg"


runFile :: String -> IO ()
runFile s = do
    file <- readFile s 
    let parsed = parseProgram file
    either putStrLn checkCST parsed

checkCST :: CstProgram -> IO ()
checkCST prog = do
    putStrLn . show $ prog
    let check = sanityCheck [prog] >> []
    if not $ null check
        then forM_ check putStrLn
        else convertCST prog

convertCST :: CstProgram -> IO ()
convertCST prog = do
    putStrLn . show $ prog
    let converted = convertProgram prog
    either putStrLn typeCheck converted

typeCheck :: ([Class], [EnumDef]) -> IO ()
typeCheck (classes, enums) = do 
    putStrLn "enums:"
    forM_ enums (putStrLn . show)
    putStrLn "classes:"
    forM_ classes (putStrLn . show)
    let typeCheck = checkTProg classes enums
    putStrLn $ show typeCheck
    return ()

