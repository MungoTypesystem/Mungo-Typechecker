module AstTransformer where

import AST
import MungoParser
import Control.Applicative (liftA2, liftA3)


-- helper data

data BuilderData = BuilderData [ClassInfo] 

data ClassInfo = ClassInfo { cName  :: String
                           , mInfo :: [MethodInfo]
                           } deriving Show

data MethodInfo = MethodInfo { mName  :: String
                             , pName  :: String
                             , fNames :: [String]
                             } deriving Show

createBuilderData :: [CstClass] -> BuilderData
createBuilderData = BuilderData . map createClassInfo 

createClassInfo :: CstClass -> ClassInfo
createClassInfo cls =
    let fieldNames = map fieldName $ classFields cls
        methodInfo = map (createMethodInfo fieldNames) $ classMethods cls
        name       = className cls
    in ClassInfo name methodInfo

createMethodInfo :: [String] -> CstMethod -> MethodInfo
createMethodInfo fields method = 
    MethodInfo (methodName method) (parameterName method) fields

-- converter

convertExpression :: MethodInfo -> CstExpression -> Either String Expression
convertExpression methodInfo exp = case exp of
    (CstExprNew cls)               -> convertNew cls
    (CstExprAssign fieldName expr) -> convertAssign methodInfo fieldName expr
    (CstExprCall ref method expr)  -> convertCall methodInfo ref method expr
    (CstExprSeq expr1 expr2)       -> convertSeq methodInfo expr1 expr2
    (CstExprIf cond texpr fexpr)   -> convertIf methodInfo cond texpr fexpr 
    (CstExprLabel name expr)       -> convertLabel methodInfo name expr
    (CstExprContinue name)         -> convertContinue methodInfo name
    (CstExprBoolConst bool)        -> convertBool methodInfo bool 
    (CstExprNull)                  -> convertNull
    (CstExprUnit)                  -> convertUnit
    (CstExprSwitch cond matches)   -> convertSwitch methodInfo cond matches
    (CstExprIdentifier str)        -> Left $ "wtf is dis" ++ str

convertNew :: String -> Either String Expression
convertNew = Right . ExprNew
  
convertAssign :: MethodInfo -> String -> CstExpression -> Either String Expression
convertAssign methodInfo fieldName expr = 
    ExprAssign fieldName <$> convertExpression methodInfo expr 

convertCall :: MethodInfo -> String -> String -> CstExpression -> Either String Expression
convertCall methodInfo reference method expr = 
    ExprCall ref method <$> convertExpression methodInfo expr
    where ref = convertReference methodInfo method

convertSeq :: MethodInfo -> CstExpression -> CstExpression -> Either String Expression
convertSeq methodInfo expr1 expr2 = 
    liftA2 ExprSeq expr1' expr2'
    where expr1' = convertExpression methodInfo expr1
          expr2' = convertExpression methodInfo expr2

convertIf :: MethodInfo -> CstExpression -> CstExpression -> CstExpression -> Either String Expression
convertIf methodInfo cond texpr fexpr = 
    liftA3 ExprIf cond' texpr' fexpr'
    where conv   = convertExpression methodInfo
          cond'  = conv cond
          texpr' = conv texpr
          fexpr' = conv fexpr 

convertLabel :: MethodInfo -> String -> CstExpression -> Either String Expression
convertLabel methodInfo name expr =
    ExprLabel name <$> convertExpression methodInfo expr

convertContinue :: MethodInfo -> String -> Either String Expression
convertContinue methodInfo name = Right $ ExprContinue name

convertBool :: MethodInfo -> Bool -> Either String Expression
convertBool methodInfo = Right . ExprBoolConst

convertNull :: Either String Expression
convertNull = Right ExprNull

convertUnit :: Either String Expression
convertUnit = Right ExprUnit

convertSwitch :: MethodInfo -> CstExpression -> [(String, CstExpression)] -> Either String Expression
convertSwitch methodInfo cond matches = 
    liftA2 ExprSwitch cond' matches'
    where cond' = convertExpression methodInfo cond
          matches'' = sequence $ map (uncurry (convertLabel methodInfo)) matches
          matches' = map (\(ExprLabel n e) -> (n, e)) <$> matches''

--convertIdentifier :: MethodInfo -> String -> Either String Expression
    
convertReference :: MethodInfo -> String -> Reference
convertReference methodInfo name 
    | name == pName methodInfo = RefParameter name
    | otherwise                = RefField name
                        

