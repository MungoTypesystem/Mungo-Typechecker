module AstTransformer where

import AST
import MungoParser
import Control.Applicative (liftA2, liftA3, (<|>))
import Control.Arrow (second)
import Data.Maybe
import Data.Either
import Debug.Trace
-- helper data

data GlobalDefinitions = GlobalDefinitions { cNames :: [String]
                                           , enumNames :: [String]
                                           , enumValues :: [String]
                                           }

type BuilderData = [ClassInfo] 

data ClassInfo = ClassInfo { cName  :: String
                           , mInfo :: [MethodInfo]
                           , recUsages :: [(String, UsageImpl)]
                           } deriving Show

data MethodInfo = MethodInfo { mName  :: String
                             , pName  :: String
                             , fNames :: [String]
                             } deriving Show

findMethodInfo :: String -> ClassInfo -> Maybe MethodInfo
findMethodInfo name (ClassInfo _ methods _) = 
    let found = filter ((== name) . mName) methods
    in if null found
        then Nothing
        else Just $ head found

convertProgram :: CstProgram -> Either String ([Class], [EnumDef])
convertProgram program = do
    let (failedClasses, convertedClasses) = partitionEithers classes
    if not $ null failedClasses
        then Left $ "failed to convert classes\n" ++ unlines failedClasses
        else Right (convertedClasses, enums)
    where  enums   = map convertEnums $ progEnums program
           classes = map (convertClass globalDefinitions classData) $ progClasses program 

           classData = createBuilderData (progClasses program)

           globalDefinitions = GlobalDefinitions classNames enumNames enumValues
           classNames = map className $ progClasses program
           enumNames  = map enumName $ progEnums program
           enumValues = concat . map enumLabels $ progEnums program

createBuilderData :: [CstClass] -> BuilderData
createBuilderData = map createClassInfo 

createClassInfo :: CstClass -> ClassInfo
createClassInfo cls =
    let fieldNames = map fieldName $ classFields cls
        methodInfo = map (createMethodInfo fieldNames) $ classMethods cls
        name       = className cls
        recUsages  = convertUsageList (classRecUsage cls)
    in ClassInfo name methodInfo recUsages

createMethodInfo :: [String] -> CstMethod -> MethodInfo
createMethodInfo fields method = 
    MethodInfo (methodName method) (parameterName method) fields

lookupRecUsages :: [ClassInfo] -> String -> Maybe [(String, UsageImpl)]
lookupRecUsages clsInfo name =
    let found = filter ((name ==) . cName) clsInfo 
    in if (null found)
            then Nothing
            else Just . recUsages $ head (found)

-- converter
convertEnums :: CstEnum -> EnumDef
convertEnums cstEnum = EnumDef (enumName cstEnum) (enumLabels cstEnum)

convertClass :: GlobalDefinitions -> BuilderData -> CstClass -> Either String Class 
convertClass global classesData cls =  do
    let (failedFields, succeededFields)   = partitionEithers fields
    let (failedMethods, succeededMethods) = partitionEithers convertedMethod 
    
    if not $ null failedFields
        then Left $ "failed to convert fields " ++ concat failedFields
        else if not $ null failedMethods 
                then Left $ "failed to convert methods " ++ concat failedMethods
                else Right $ Class name (Usage usage recursiveU) succeededFields succeededMethods
    where 
          usage           = convertUsage (classUsage cls)
          recursiveU      = convertUsageList (classRecUsage cls)
          classInfo       = createClassInfo cls
          name            = cName classInfo
          fields          = map (convertField global) (classFields cls)
          classData       = filter ((== className cls). cName) classesData
          convertedMethod = map (convertMethod global classesData (head classData) recursiveU) (classMethods cls)

convertMethod :: GlobalDefinitions -> BuilderData -> ClassInfo -> [(String, UsageImpl)] -> CstMethod  -> Either String Method 
convertMethod global classesData classInfo recUsages method = do
    mType'  <-  mType
    mpType' <- mpType
    expr'   <- expr
    Right $ Method mType' mName mpType' mpName expr' 
    where className   = cName classInfo
          mName       = methodName method
          mType       = convertType global (methodType method) mTypeUsage 
          mTypeUsage  = methodTypeUsage method >>=
                        \cstUsage -> Just $ Usage (convertUsage cstUsage) recUsages
          mpName      = parameterName method
          mpType      = convertType global (parameterType method) mpTypeUsage 
          mpTypeUsage = parameterTypeUsage method >>= \cstUsage -> 
                        lookupRecUsages classesData (parameterType method) >>= \recUsages' -> 
                        Just $ Usage (convertUsage cstUsage) recUsages'

          mInfo       = fromMaybe (Left "unable to find methodInfo") $ Right <$> mName `findMethodInfo` classInfo 
          expr        = mInfo >>= \mInfo' -> convertExpression global mInfo' (methodExpr method)
          
convertType :: GlobalDefinitions -> String -> Maybe Usage -> Either String Type 
convertType global typeStr u
    | typeStr == "void"                 = Right $ BType VoidType
    | typeStr == "bool"                 = Right $ BType BoolType 
    | typeStr `elem` (enumNames global) = Right $ BType $ EnumType typeStr
    | typeStr `elem` (cNames global)    = if (isJust u) 
                                            then Right (CType (typeStr, (fromJust u))) 
                                            else Left $ "unable to convert to class type " ++ typeStr ++ " "
    | otherwise                         = Left $ "unable to convert to Type " ++ typeStr ++ " "
    
convertField :: GlobalDefinitions -> CstField -> Either String Field
convertField global field = liftA2 Field fieldType' (return (fieldName field))
    where fieldType' = convertFieldType global (fieldType field)

convertFieldType :: GlobalDefinitions -> String -> Either String FieldType
convertFieldType global field =   BaseFieldType <$> convertBaseType global field
                              <|> convertClassTypeField global field

convertClassTypeField :: GlobalDefinitions -> String -> Either String FieldType
convertClassTypeField global name
    | name `elem` (cNames global) = Right $ ClassFieldType name
    | otherwise                   = Left $ "Unknown field " ++ name

convertBaseType :: GlobalDefinitions -> String -> Either String BaseType
convertBaseType global field 
    | field == "void"                 = Right $ VoidType
    | field == "bool"                 = Right $ BoolType
    | field `elem` (enumNames global) = Right $ EnumType field
    | otherwise                       = Left "cannot convert to base type"

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
    where ref = convertReference global methodInfo reference 

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
            (ExprCall ref' _ expr') -> ExprSwitch ref' call' <$> matches'
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
    | name `elem` fNames methodInfo = Right $ RefField name
    | otherwise                     = Left $ "field not found " ++ name

convertToLiteral :: GlobalDefinitions -> MethodInfo -> String -> Either String Expression
convertToLiteral global methodInfo name 
    | name `elem` enumValues global = Right $ ExprLitteral name
    | True                          = Left $ "literal not found " ++ name

convertReference :: GlobalDefinitions -> MethodInfo -> String -> Either String Reference
convertReference global methodInfo name =   convertToParameter global methodInfo name
                                        <|> convertToField global methodInfo name
                                        <|> Left (concat ["unable to convert ", name, " to reference"])
                        
