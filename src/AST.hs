module AST where

type ClassName = String
type FieldName = String
type MethodName = String
type ParameterName = String
type ObjectName = String
type LabelName = String
type UsageVarName = String
type GenericClassName = String
type GenericUsageName = String
data BaseType = BoolType | VoidType | EnumType String deriving (Show, Eq)


data ClassGenericType = ClassNoGeneric 
                      | ClassGeneric GenericClassName  GenericUsageName 
                   deriving (Show, Eq)

type Typestate = (ClassName, GenericInstance, Usage)

data GenericInstance = GenericBot 
                     | GenericInstance { genericName      :: ClassName
                                       , genericRecursive :: GenericInstance 
                                       , genericUsage     :: Usage
                                       } 
                     | GenericClass GenericClassName GenericUsageName
                        deriving (Show, Eq)

data FieldType = ClassFieldGen ClassName GenericInstance
               | BaseFieldType BaseType
               | GenericField 
               deriving (Show, Eq)

data Type = BType BaseType 
          | CType Typestate
          | BotType
          | GType 
          deriving (Show, Eq)

data EnumDef = EnumDef String [LabelName]
                deriving (Show)

data Class = Class { cname    :: ClassName
                   , cGeneric :: ClassGenericType
                   , cusage   :: Usage
                   , cfields  :: [Field]
                   , cmethods :: [Method]
                   } deriving (Show)

data Usage = Usage { current :: UsageImpl
                   , recursiveUsages :: [(String, UsageImpl)]
                   } 
           | UsageTop
                deriving (Show, Eq)

data UsageImpl = UsageChoice [(String, UsageImpl)]
               | UsageBranch [(String, UsageImpl)]
               | UsageVariable String
               | UsageEnd
               | UsageGenericVariable
                 deriving (Show, Eq)

data Field = Field { ftype :: FieldType
                   , fname :: FieldName 
                   } deriving (Show)

data Method = Method { rettype :: Type
                     , mname   :: MethodName
                     , partype :: Type
                     , parname :: ParameterName
                     , mexpr   :: Expression
                     } deriving (Show)

data Reference = RefParameter ParameterName
               | RefField FieldName
                 deriving (Show)

data Expression = ExprNew ClassName GenericInstance
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


