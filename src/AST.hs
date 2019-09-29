module AST where

type ClassName = String
type FieldName = String
type MethodName = String
type ParameterName = String
type ObjectName = String
data BaseType = BoolType | VoidType deriving (Show, Eq)

type Typestate = (ClassName, Usage)

data ClassDeclType = SimpleClassType ClassName
                   | ComplexClassType Typestate
                   deriving (Show, Eq)

data FieldType = ClassFieldType Typestate
               | BaseFieldType BaseType
               deriving (Show, Eq)

data Type = BType BaseType 
          | CType ClassDeclType
          deriving (Show, Eq)

data Class = Class ClassName Usage [Field] [Method]
             deriving (Show)

data Usage = UsageChoice [(String, Usage)]
           | UsageBranch [(String, Usage)]
           | UsageRecursive String Usage
           | UsageEnd
             deriving (Show, Eq)

data Field = Field Type FieldName 
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
                  deriving (Show)


