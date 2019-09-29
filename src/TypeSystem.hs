module TypeSystem where

import MungoParser
import AST

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
envLookup = flip lookup

agree :: Type -> Type -> Bool
agree (BType b1) (BType b2) = b1 == b2
agree (CType (SimpleClassType cn1)) (CType (ComplexClassType (cn2, _))) = cn1 == cn2
agree _ _ = False 
