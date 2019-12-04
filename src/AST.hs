module AST where

import Data.List (intercalate)
import Data.Maybe (maybe)

type ClassName = String
type FieldName = String
type MethodName = String
type ParameterName = String
type ObjectName = String
type LabelName = String
type UsageVarName = String
type GenericClassName = String
type GenericUsageName = String
data BaseType = BoolType | VoidType | EnumType String deriving (Show, Eq, Ord)


data ClassGenericType = ClassNoGeneric 
                      | ClassGeneric GenericClassName  GenericUsageName 
                   deriving (Show, Eq, Ord)

type Typestate = (ClassName, GenericInstance, Usage)

data GenericInstance = GenericBot 
                     | GenericInstance { genericName      :: ClassName
                                       , genericRecursive :: GenericInstance 
                                       , genericUsage     :: Usage
                                       } 
                     | GenericClass GenericClassName GenericUsageName
                        deriving (Show, Eq, Ord)

data FieldType = ClassFieldGen ClassName GenericInstance
               | BaseFieldType BaseType
               | GenericField 
               deriving (Show, Eq)

data Type = BType BaseType 
          | CType Typestate
          | BotType
          | GType 
          deriving (Show, Ord)

isEnd :: Usage -> Bool
isEnd u = isEnd' (current u) (recursiveUsages u)
    where isEnd' (UsageEnd)        _    = True
          isEnd' (UsageVariable x) recu = maybe False ((flip isEnd') recu) (x `lookup` recu)
          isEnd' _                 _    = False


instance Eq Type where
    (BType b1)        == (BType b2)         = b1 == b2
    (BotType)         == (BotType)          = True
    (GType)           == (GType)            = True
    (CType t1)        == (CType t2)         = t1 == t2
    (BotType)         == (CType (_, _, u))  = isEnd u
    (CType (_, _, u)) == (BotType)          = isEnd u
    _                 == _                  = False
    

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
           | UsageInference
                deriving (Eq, Ord)

data UsageImpl = UsageChoice [(String, UsageImpl)]
               | UsageBranch [(String, UsageImpl)]
               | UsageVariable String
               | UsageEnd
               | UsageGenericVariable
                 deriving (Eq, Ord)

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

showStringUsageImpl :: [(String, UsageImpl)] -> String -> String
showStringUsageImpl l eq = 
    intercalate " " (map (\(p, u) -> p ++ eq ++ show u) l)

instance Show Usage where
    show (Usage c rec) = show c ++ "[ "++ showStringUsageImpl rec " = " ++ " ]"
    show (UsageTop)       = "(top)"
    show (UsageInference) = "(inf)"

instance Show UsageImpl where
    show (UsageChoice ls) = "< " ++ showStringUsageImpl ls ": "++ " >"
    show (UsageBranch ls) = "{ " ++ showStringUsageImpl ls "; "++ " }"
    show (UsageVariable x)= x
    show (UsageEnd)       = "end"
    show (UsageGenericVariable) = "(gen)"



