
module MungoParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data AExpr = IntConst Integer
            | ABinary ABinOp AExpr AExpr
              deriving (Show)

data BExpr = BoolConst Bool deriving (Show)

data ABinOp = Add
             | Subtract
             | Multiply
             | Divide
               deriving (Show)

data Expr = Seq [Expr]
           | Assign String AExpr
           | If BExpr Expr Expr
           | Continue String
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
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", ":="]
            }