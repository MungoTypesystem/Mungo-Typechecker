
module MungoParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Values = BoolConst Bool
            | Unit
            | Label String
            | Null deriving (Show)

data BaseTypes = BaseTypeVoid
               | BaseTypeBool deriving (Show) 
 
data Expr = Seq [Expr]
          | Assign String Expr
          | If BExpr Expr Expr
          | Continue Label
          | Value deriving (Show)

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