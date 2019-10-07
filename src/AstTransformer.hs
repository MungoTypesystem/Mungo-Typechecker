module AstTransformer where

import AST
import MungoParser
import Control.Applicative (liftA2, liftA3, (<|>))
import Control.Arrow (second)


-- helper data

data GlobalDefinitions = GlobalDefinitions { cNames :: [String]
                                           , enumNames :: [String]
                                           , enumValues :: [String]
                                           }

data BuilderData = BuilderData [ClassInfo] 

data ClassInfo = ClassInfo { cName  :: String
                           , mInfo :: [MethodInfo]
                           } deriving Show

data MethodInfo = MethodInfo { mName  :: String
                             , pName  :: String
                             , fNames :: [String]
                             } deriving Show

convertProgram :: CstProgram -> Either String ([Class], [EnumDef])
convertProgram program = Left $ "the frick"
   where  enums   = map convertEnums $ progEnums program
          classes = map (convertClass globalDefinitions) $ progClasses program 

          globalDefinitions = GlobalDefinitions classNames enumNames enumValues
          classNames = map className $ progClasses program
          enumNames  = map enumName $ progEnums program
          enumValues = concat . map enumLabels $ progEnums program

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
convertEnums :: CstEnum -> EnumDef
convertEnums cstEnum = EnumDef (enumName cstEnum) (enumLabels cstEnum)

convertClass :: GlobalDefinitions -> CstClass -> Class 
convertClass definitions cls = error "not implemeneted"
    where usage     = convertUsage (classUsage cls)
          classInfo = createClassInfo cls
          

convertUsage :: CstUsage -> UsageImpl
convertUsage (CstUsageChoice xs)  = UsageChoice $ convertUsageList xs
convertUsage (CstUsageBranch xs)  = UsageBranch $ convertUsageList xs
convertUsage (CstUsageVariable v) = UsageVariable v
convertUsage (CstUsageEnd)        = UsageEnd

convertUsageList :: [(String, CstUsage)] -> [(String, UsageImpl)]
convertUsageList = map (second convertUsage) 


convertExpression :: GlobalDefinitions -> MethodInfo -> CstExpression -> Either String Expression
convertExpression global methodInfo exp = case exp of
    (CstExprNew cls)               -> convertNew cls
    (CstExprAssign fieldName expr) -> convertAssign global methodInfo fieldName expr
    (CstExprCall ref method expr)  -> convertCall global methodInfo ref method expr
    (CstExprSeq expr1 expr2)       -> convertSeq global methodInfo expr1 expr2
    (CstExprIf cond texpr fexpr)   -> convertIf global methodInfo cond texpr fexpr 
    (CstExprLabel name expr)       -> convertLabel global methodInfo name expr
    (CstExprContinue name)         -> convertContinue global methodInfo name
    (CstExprBoolConst bool)        -> convertBool global methodInfo bool 
    (CstExprNull)                  -> convertNull
    (CstExprUnit)                  -> convertUnit
    (CstExprSwitch cond matches)   -> convertSwitch global methodInfo cond matches
    (CstExprIdentifier str)        -> convertIdentifier global methodInfo str

convertNew :: String -> Either String Expression
convertNew = Right . ExprNew
  
convertAssign :: GlobalDefinitions -> MethodInfo -> String -> CstExpression -> Either String Expression
convertAssign global methodInfo fieldName expr = 
    ExprAssign fieldName <$> convertExpression global methodInfo expr 

convertCall :: GlobalDefinitions -> MethodInfo -> String -> String -> CstExpression -> Either String Expression
convertCall global methodInfo reference method expr = 
    ExprCall <$> ref <*> return method <*> convertExpression global methodInfo expr
    where ref = convertReference global methodInfo method

convertSeq :: GlobalDefinitions -> MethodInfo -> CstExpression -> CstExpression -> Either String Expression
convertSeq global methodInfo expr1 expr2 = 
    liftA2 ExprSeq expr1' expr2'
    where expr1' = convertExpression global methodInfo expr1
          expr2' = convertExpression global methodInfo expr2

convertIf :: GlobalDefinitions -> MethodInfo -> CstExpression -> CstExpression -> CstExpression -> Either String Expression
convertIf global methodInfo cond texpr fexpr = 
    liftA3 ExprIf cond' texpr' fexpr'
    where conv   = convertExpression global methodInfo
          cond'  = conv cond
          texpr' = conv texpr
          fexpr' = conv fexpr 

convertLabel :: GlobalDefinitions -> MethodInfo -> String -> CstExpression -> Either String Expression
convertLabel global methodInfo name expr =
    ExprLabel name <$> convertExpression global methodInfo expr

convertContinue :: GlobalDefinitions -> MethodInfo -> String -> Either String Expression
convertContinue global methodInfo name = Right $ ExprContinue name

convertBool :: GlobalDefinitions -> MethodInfo -> Bool -> Either String Expression
convertBool global methodInfo = Right . ExprBoolConst

convertNull :: Either String Expression
convertNull = Right ExprNull

convertUnit :: Either String Expression
convertUnit = Right ExprUnit

convertSwitch :: GlobalDefinitions -> MethodInfo -> CstExpression -> [(String, CstExpression)] -> Either String Expression
convertSwitch global methodInfo expr matches =  
    do call' <- call
       case call' of 
            (ExprCall ref' _ expr') -> ExprSwitch ref' expr' <$> matches'
            _                       -> Left $ "failed to convert call"
    where (CstExprCall ref method param) = expr
          call      = convertCall global methodInfo ref method param
          matches'' = sequence $ map (uncurry (convertLabel global methodInfo)) matches
          matches'  = map (\(ExprLabel n e) -> (n, e)) <$> matches''

convertIdentifier :: GlobalDefinitions ->  MethodInfo -> String -> Either String Expression
convertIdentifier global methodInfo str = parameter' <|> field' <|> literal'
    where parameter' = ExprReference <$> convertToParameter global methodInfo str
          field'     = ExprReference <$> convertToField global methodInfo str
          literal'   = convertToLiteral global methodInfo str
    
convertToParameter :: GlobalDefinitions -> MethodInfo -> String -> Either String Reference 
convertToParameter global methodInfo name 
    | name == pName methodInfo = Right $ RefParameter name
    | otherwise                = Left $ "paramenter not found " ++ name

convertToField :: GlobalDefinitions -> MethodInfo -> String -> Either String Reference 
convertToField global methodInfo name
    | name `elem` fNames methodInfo = Right $ RefParameter name
    | otherwise                     = Left $ "field not found " ++ name

convertToLiteral :: GlobalDefinitions -> MethodInfo -> String -> Either String Expression
convertToLiteral global methodInfo name = 
    Left "literal not found"

convertReference :: GlobalDefinitions -> MethodInfo -> String -> Either String Reference
convertReference global methodInfo name =   convertToParameter global methodInfo name
                                        <|> convertToField global methodInfo name
                                        <|> Left (concat ["unable to convert ", name, " to reference"])
                        

