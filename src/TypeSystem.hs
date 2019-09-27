module TypeSystem where

import MungoParser

type ClassName = String
type FieldName = String
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

-- EnvTF
type FieldTypeEnv = [(FieldName, FieldType)] 

-- Λ
type ObjectFieldTypeEnv = [(ObjectName, (ClassName, Type), FieldTypeEnv)]

-- EnvTO
type ObjectTypeEnv = [(ObjectName, Typestate)]

-- EnvTS
type ParameterStackTypeEnv = [(ObjectName, (ParameterName, FieldType))]

-- Δ
type Delta = [(ObjectTypeEnv, ParameterStackTypeEnv)]

envLookup :: [(String, a)] -> String -> Maybe a
envLookup [] _ = Nothing
envLookup ((n, x):ns) m = if n == m then Just x else envLookup ns n

agree :: Type -> Type -> Bool
agree (BType b1) (BType b2) = b1 == b2
agree (CType (SimpleClassType cn1)) (CType (ComplexClassType (cn2, _))) = cn1 == cn2
agree _ _ = False