module AST where

type ClassName = String
type FieldName = String
type MethodName = String
type ParameterName = String
type ObjectName = String
data BaseType = BoolType | VoidType deriving (Show, Eq)

type Typestate = (ClassName, Usage)

data FieldType = ClassFieldType ClassName
               | BaseFieldType BaseType
               deriving (Show, Eq)

data Type = BType BaseType 
          | CType Typestate
          | BotType
          deriving (Show, Eq)

data Class = Class {
                     cname    :: ClassName,
                     cusage   :: Usage,
                     cfields  :: [Field],
                     cmethods :: [Method]
                   } deriving (Show)

data Usage = UsageChoice [(String, Usage)]
           | UsageBranch [(String, Usage)]
           | UsageRecursive String Usage
           | UsageEnd
             deriving (Show, Eq)

data Field = Field { 
                     ftype :: FieldType,
                     fname :: FieldName 
                   }
             deriving (Show)

data Method = Method Type MethodName Type ParameterName Expression 
              deriving (Show)

data Reference = RefParameter ParameterName
               | RefField FieldName
                 deriving (Show)

data Expression = ExprNew ClassName 
                | ExprAssign FieldName Expression
                | ExprCall Reference MethodName Expression
                | ExprSeq Expression Expression
                | ExprIf Expression Expression Expression
                | ExprLabel String Expression 
                | ExprContinue String
                | ExprBoolConst Bool
                | ExprNull
                | ExprUnit
                | ExprIdentifier String 
                | ExprReturn Expression 
                | ExprParameter String 
                | ExprFld String
                | ExprLitteral String
                | ExprObjectName String
                  deriving (Show)


