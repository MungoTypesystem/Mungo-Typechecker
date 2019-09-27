module MungoParser where

import System.IO
import Control.Monad
import Text.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Class = Class String Usage [Field] [Method]
             deriving (Show)

data Usage = UsageChoice [(String, Usage)]
           | UsageBranch [(String, Usage)]
           | UsageRecursive String Usage
           | UsageVariable String
           | UsageEnd
             deriving (Show, Eq)

data Field = Field String String 
             deriving (Show)

data Method = Method String String String String Expression 
             deriving (Show)

data Expression = ExprNew String
          | ExprAssign String Expression
          | ExprCall String String Expression
          | ExprSeq Expression Expression
          | ExprIf Expression Expression Expression
          | ExprLabel String Expression 
          | ExprContinue String
          | ExprBoolConst Bool
          | ExprNull
          | ExprUnit
          | ExprIdentifier String 
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
                                      , "end" ]
            , Token.reservedOpNames = ["=", ":"]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
angles     = Token.angles     lexer -- 
braces     = Token.braces     lexer -- 
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
semi       = Token.semi       lexer -- parses a semicolon
colon      = Token.colon      lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
dot        = Token.dot        lexer


parseProgram = parse parseClass "" 

parseClass :: Parser Class
parseClass =
    do reserved "class" 
       className <- identifier 
       braces $ do 
            usage   <- parseUsage
            fields  <- parseFields
            methods <- parseMethods
            return $ Class className usage fields methods 

testParse :: Parser (Usage, [Field], [Method])
testParse = do
    fields <- parseFields
    methods <- parseMethods
    return $ (UsageEnd, fields, methods)

parseMethods :: Parser [Method]
parseMethods = parseMethod `manyTill` lookAhead (reserved "}")

parseMethod :: Parser Method
parseMethod =
    do returnType <- identifier
       methodName <- identifier
       (parameterType, parameterName) <- parens parseParameter
       body       <- braces parseExpr
       return $ Method returnType methodName parameterType parameterName body
    where parseParameter :: Parser (String, String)
          parseParameter = 
                do paramenterType <- identifier
                   parameterName <- identifier
                   return (paramenterType, parameterName)
        

parseFields :: Parser [Field]
parseFields =  parseField `manyTill` lookAhead (try parseMethod)

parseField :: Parser Field
parseField =
    do fieldtype <- identifier
       fieldname <- identifier
       --semi
       return $ Field fieldtype fieldname
       

parseUsage :: Parser Usage
parseUsage = parseRecursiveUsage
    
parseRecursiveUsage :: Parser Usage
parseRecursiveUsage =   parseRecursiveUsage' 
                    <|> parseBranchUsage

parseRecursiveUsage' :: Parser Usage
parseRecursiveUsage' = 
    do reserved "rec"
       name <- identifier
       dot
       usage <- parseRecursiveUsage
       return $ UsageRecursive name usage

-- u
parseBranchUsage :: Parser Usage
parseBranchUsage =   parseEndUsage 
                 <|> braces parseBranchUsage'
                 <|> parseVariableUsage

parseBranchUsage' :: Parser Usage
parseBranchUsage' = 
    do list <- many1 parseBranchPairUsage
       return $ UsageBranch list

parseBranchPairUsage :: Parser (String, Usage)
parseBranchPairUsage = 
    do name <- identifier
       semi
       usage <- parseChoiceUsage 
       return $ (name, usage)

-- w
parseChoiceUsage :: Parser Usage
parseChoiceUsage =   angles parseChoiceUsage'
                 <|> parseBranchUsage
       
parseChoiceUsage' :: Parser Usage
parseChoiceUsage' = 
    do list <- many1 parseChoicePairUsage
       return $ UsageChoice list

parseChoicePairUsage :: Parser (String, Usage)
parseChoicePairUsage = 
    do name <- identifier
       colon 
       usage <- parseUsage
       return $ (name, usage)
    
parseVariableUsage :: Parser Usage
parseVariableUsage = 
    do name <- identifier
       return $ UsageVariable name

parseEndUsage :: Parser Usage
parseEndUsage = reserved "end" >> return UsageEnd

parseExpr :: Parser Expression
parseExpr =   parseSeqExpr 
          <|> parseExpr'

parseExpr' :: Parser Expression
parseExpr' = parens parseExpr
           <|> try parseLabelExpr 
           <|> parseNewExpr
           <|> try parseAssignExpr
           <|> parseIfExpr
           <|> parseContinueExpr
           <|> parseConstValuesExpr
           <|> parseIdentifierExpr

parseNewExpr :: Parser Expression
parseNewExpr =
    do reserved "new"
       var <- identifier
       return $ ExprNew var

parseAssignExpr :: Parser Expression
parseAssignExpr = 
    do var <- identifier
       reserved "="
       expr <- parseExpr 
       return $ ExprAssign var expr 

parseSeqExpr :: Parser Expression
parseSeqExpr =
    do list <- (sepBy1 parseExpr' semi)
       return $ unfoldSeqExpr list

unfoldSeqExpr :: [Expression] -> Expression
unfoldSeqExpr list = 
    case length list of
        1 -> head list
        n -> ExprSeq (head list) (unfoldSeqExpr (tail list))
   
       
parseIfExpr :: Parser Expression
parseIfExpr =
    do reserved "if"
       cond <- parens parseExpr
       expr1 <- braces parseExpr
       expr2 <- braces parseExpr
       return $ ExprIf cond expr1 expr2

parseLabelExpr :: Parser Expression
parseLabelExpr =
    do label <- identifier
       reserved ":"
       expr <- parseExpr
       return $ ExprLabel label expr
       
parseContinueExpr :: Parser Expression
parseContinueExpr =
    do reserved "continue"
       label <- identifier
       return $ ExprContinue label
       
parseConstValuesExpr :: Parser Expression
parseConstValuesExpr =
        (reserved "true"  >> return (ExprBoolConst True))
    <|> (reserved "false" >> return (ExprBoolConst False))
    <|> (reserved "null"  >> return ExprNull)
    <|> (reserved "unit"  >> return ExprUnit)

parseIdentifierExpr :: Parser Expression
parseIdentifierExpr = 
    do str <- identifier
       return $ ExprIdentifier str 


parseFile :: String -> IO String
parseFile f = do
    t <- readFile f
    return $ case parseProgram t of
        Right prog -> show $ prog 
        Left  err  -> show $ err
