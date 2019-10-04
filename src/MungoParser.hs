module MungoParser( CstClass (CstClass, className, classFields, classMethods)
                  , CstUsage (CstUsageEnd, CstUsageChoice, CstUsageBranch)
                  , CstField (CstField, fieldType, fieldName)
                  , CstMethod (CstMethod, methodName, methodType, parameterName, parameterType)
                  , CstExpression (CstExprNew, CstExprAssign, CstExprCall, CstExprSeq, CstExprIf, CstExprLabel, CstExprContinue, CstExprBoolConst, CstExprNull, CstExprUnit, CstExprSwitch, CstExprIdentifier)
                  , parseProgram
                  , testSwitch ) where

import System.IO
import Control.Monad
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Arrow (left)

testSwitch = parse parseSwitchExpr ""


data CstClass = CstClass { className :: String
                         , classUsage :: CstUsage
                         , classRecUsage :: [(String, CstUsage)]
                         , classFields :: [CstField]
                         , classMethods :: [CstMethod]
                         }
                deriving (Show)

type CstRecUsage = (String, CstUsage)

data CstUsage = CstUsageChoice [(String, CstUsage)]
              | CstUsageBranch [(String, CstUsage)]
              | CstUsageVariable String
              | CstUsageEnd
                deriving (Show, Eq)

data CstField = CstField { fieldType :: String
                         , fieldName :: String 
                         } 
                deriving (Show)

data CstMethod = CstMethod { methodType :: String
                           , methodName :: String
                           , parameterType :: String
                           , parameterName :: String
                           , methodExpr :: CstExpression
                           }
                 deriving (Show)

data CstExpression = CstExprNew String
                   | CstExprAssign String CstExpression
                   | CstExprCall String String CstExpression
                   | CstExprSeq CstExpression CstExpression
                   | CstExprIf CstExpression CstExpression CstExpression
                   | CstExprLabel String CstExpression 
                   | CstExprContinue String
                   | CstExprBoolConst Bool
                   | CstExprNull
                   | CstExprUnit
                   | CstExprSwitch CstExpression [(String, CstExpression)]
                   | CstExprIdentifier String 
                     deriving (Show)

languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "continue"
                                      , "true"
                                      , "false"
                                      , "new"
                                      , "rec"
                                      , "class"
                                      , "end"
                                      , "switch" ]
            , Token.reservedOpNames = ["=", ":"]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
angles     = Token.angles     lexer -- 
brackets   = Token.brackets   lexer -- 
braces     = Token.braces     lexer -- 
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
semi       = Token.semi       lexer -- parses a semicolon
colon      = Token.colon      lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
dot        = Token.dot        lexer


parseProgram :: String -> Either String CstClass
parseProgram  = left show . parse parseClass "" 

parseClass :: Parser CstClass
parseClass =
    do reserved "class" 
       className <- identifier 
       braces $ do 
            (usage, recursiveUsage) <- parseUsage
            fields                  <- parseFields
            methods                 <- parseMethods
            return $ CstClass className usage recursiveUsage fields methods 

parseMethods :: Parser [CstMethod]
parseMethods = parseMethod `manyTill` lookAhead (reserved "}")

parseMethod :: Parser CstMethod
parseMethod =
    do returnType <- identifier
       methodName <- identifier
       (parameterType, parameterName) <- parens parseParameter
       body       <- braces parseExpr
       return $ CstMethod returnType methodName parameterType parameterName body
    where parseParameter :: Parser (String, String)
          parseParameter = 
                do paramenterType <- identifier
                   parameterName <- identifier
                   return (paramenterType, parameterName)
        

parseFields :: Parser [CstField]
parseFields =  parseField `manyTill` lookAhead (try parseMethod)

parseField :: Parser CstField
parseField =
    do fieldtype <- identifier
       fieldname <- identifier
       return $ CstField fieldtype fieldname
       

parseUsage :: Parser (CstUsage, [(String, CstUsage)])
parseUsage = do
    usage <- parseBranchUsage 
    recursiveUsages <- brackets (many parseRecursiveUsage)
    return $ (usage, recursiveUsages)
    
parseRecursiveUsage :: Parser (String, CstUsage)
parseRecursiveUsage = do
    name <- identifier
    reserved "="
    usage <- parseBranchUsage
    return $ (name, usage)

-- u
parseBranchUsage :: Parser CstUsage
parseBranchUsage =   parseEndUsage 
                 <|> braces parseBranchUsage'
                 <|> parseVariableUsage

parseBranchUsage' :: Parser CstUsage
parseBranchUsage' = 
    do list <- many1 parseBranchPairUsage
       return $ CstUsageBranch list

parseBranchPairUsage :: Parser (String, CstUsage)
parseBranchPairUsage = 
    do name <- identifier
       semi
       usage <- parseChoiceUsage 
       return $ (name, usage)

-- w
parseChoiceUsage :: Parser CstUsage
parseChoiceUsage =   angles parseChoiceUsage'
                 <|> parseBranchUsage
       
parseChoiceUsage' :: Parser CstUsage
parseChoiceUsage' = 
    do list <- many1 parseChoicePairUsage
       return $ CstUsageChoice list

parseChoicePairUsage :: Parser (String, CstUsage)
parseChoicePairUsage = 
    do name <- identifier
       colon 
       usage <- parseBranchUsage 
       return $ (name, usage)
    
parseVariableUsage :: Parser CstUsage
parseVariableUsage = 
    do name <- identifier
       return $ CstUsageVariable name

parseEndUsage :: Parser CstUsage
parseEndUsage = reserved "end" >> return CstUsageEnd

parseExpr :: Parser CstExpression
parseExpr =   parseSeqExpr 
          <|> parseExpr'

parseExpr' :: Parser CstExpression
parseExpr' = parens parseExpr
           <|> try parseLabelExpr 
           <|> parseNewExpr
           <|> try parseAssignExpr
           <|> try parseCallExpr
           <|> parseIfExpr
           <|> parseContinueExpr
           <|> parseSwitchExpr
           <|> parseConstValuesExpr
           <|> parseIdentifierExpr

parseCallExpr :: Parser CstExpression
parseCallExpr = 
    do ref <- identifier
       reservedOp "."
       fname <- identifier
       expr <- parens $ parseExpr
       return $ CstExprCall ref fname expr

parseNewExpr :: Parser CstExpression
parseNewExpr =
    do reserved "new"
       var <- identifier
       return $ CstExprNew var

parseAssignExpr :: Parser CstExpression
parseAssignExpr = 
    do var <- identifier
       reserved "="
       expr <- parseExpr 
       return $ CstExprAssign var expr 

parseSeqExpr :: Parser CstExpression
parseSeqExpr =
    do list <- (sepBy1 parseExpr' semi)
       return $ unfoldSeqExpr list

unfoldSeqExpr :: [CstExpression] -> CstExpression
unfoldSeqExpr list = 
    case length list of
        1 -> head list
        n -> CstExprSeq (head list) (unfoldSeqExpr (tail list))
   
       
parseIfExpr :: Parser CstExpression
parseIfExpr =
    do reserved "if"
       cond <- parens parseExpr
       expr1 <- braces parseExpr
       expr2 <- braces parseExpr
       return $ CstExprIf cond expr1 expr2

parseLabelExpr :: Parser CstExpression
parseLabelExpr =
    do label <- identifier
       reserved ":"
       expr <- parseExpr
       return $ CstExprLabel label expr
       
parseContinueExpr :: Parser CstExpression
parseContinueExpr =
    do reserved "continue"
       label <- identifier
       return $ CstExprContinue label
       
parseSwitchExpr :: Parser CstExpression
parseSwitchExpr =
    do reserved "switch"
       cond  <- parens $ parseCallExpr 
       exprs <- braces $ many parseLabelExpr
       let exprs' = map (\(CstExprLabel str expr) -> (str, expr)) exprs
       return $ CstExprSwitch cond exprs'


parseConstValuesExpr :: Parser CstExpression
parseConstValuesExpr =
        (reserved "true"  >> return (CstExprBoolConst True))
    <|> (reserved "false" >> return (CstExprBoolConst False))
    <|> (reserved "null"  >> return CstExprNull)
    <|> (reserved "unit"  >> return CstExprUnit)



parseIdentifierExpr :: Parser CstExpression
parseIdentifierExpr = 
    do str <- identifier
       return $ CstExprIdentifier str 

