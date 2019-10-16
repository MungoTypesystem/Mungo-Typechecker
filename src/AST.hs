module AST where

type ClassName = String
type FieldName = String
type MethodName = String
type ParameterName = String
type ObjectName = String
type LabelName = String
type UsageVarName = String
data BaseType = BoolType | VoidType | EnumType String deriving (Show, Eq)

type Typestate = (ClassName, Usage)

data FieldType = ClassFieldType ClassName
               | BaseFieldType BaseType
               deriving (Show, Eq)

data Type = BType BaseType 
          | CType Typestate
          | BotType
          deriving (Show, Eq)

data EnumDef = EnumDef String [LabelName]
                deriving (Show)

data Class = Class {
                     cname    :: ClassName,
                     cusage   :: Usage,
                     cfields  :: [Field],
                     cmethods :: [Method]
                   } deriving (Show)

data Usage = Usage { current :: UsageImpl
                   , recursiveUsages :: [(String, UsageImpl)]
                   } deriving (Show, Eq)

data UsageImpl = UsageChoice [(String, UsageImpl)]
               | UsageBranch [(String, UsageImpl)]
               | UsageVariable String
               | UsageEnd
                 deriving (Show, Eq)

data Field = Field { 
                     ftype :: FieldType,
                     fname :: FieldName 
                   }
             deriving (Show)

data Method = Method {
                    rettype :: Type, 
                    mname :: MethodName,
                    partype :: Type,
                    parname :: ParameterName,
                    mexpr :: Expression
                    }
              deriving (Show)

data Reference = RefParameter ParameterName
               | RefField FieldName
                 deriving (Show)

data Expression = ExprNew ClassName 
                | ExprAssign FieldName Expression
                | ExprCall Reference MethodName  Expression
                | ExprSeq Expression Expression
                | ExprIf Expression Expression Expression
                | ExprLabel String Expression 
                | ExprContinue String
                | ExprBoolConst Bool
                | ExprNull
                | ExprUnit
                | ExprSwitch Reference Expression [(String, Expression)]
                | ExprReturn Expression 
                | ExprReference Reference
                | ExprLitteral String
                | ExprObjectName String
                  deriving (Show)


