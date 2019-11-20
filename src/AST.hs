module AST where

import Data.List (intercalate)

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

instance Show Reference where
    show (RefParameter pname) = "(par)" ++ pname
    show (RefField pname)     = "(fld)" ++ pname

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

instance Show Expression where
    show (ExprNew n g)    = "new " ++ n ++ "<" ++ show g ++ ">"
    show (ExprAssign f e)   = f ++ " = " ++ show e
    show (ExprCall r n e)   = show r ++ "." ++ n ++ "( " ++ show e ++ " )"
    show (ExprSeq e1 e2)    = show e1 ++ ";" ++ show e2
    show (ExprIf c e1 e2)   = "if ( " ++ show c ++ " ) {" ++ show e1 ++ "} else {" ++ show e2 ++ "}"
    show (ExprLabel s e)    = "(lbl)" ++ s ++ ": (" ++ show e ++ ")"
    show (ExprContinue l)   = "continue " ++ l
    show (ExprBoolConst b)  = show b
    show (ExprNull)         = "null"
    show (ExprUnit)         = "unit"
    show (ExprSwitch r e l) = "switch( " ++ show e ++ " ){" ++ intercalate ", " (map (\(l, e') -> show (ExprLabel l e')) l) ++ "}"
    show (ExprReturn e)     = "return " ++ show e
    show (ExprReference r)  = show r
    show (ExprLitteral s)   = "(lit)" ++ s
    show (ExprObjectName s) = "(obj)" ++ show s

