module AstTransformer where

import AST
import MungoParser
import Control.Applicative (liftA2, liftA3, (<|>))
import Control.Arrow (second)
import Data.Maybe
import Data.Either
import Debug.Trace
-- helper data

(<?>) :: Maybe b -> a -> Either a b
v <?> s = fromMaybe (Left s) $ Right <$> v

data GlobalDefinitions = GlobalDefinitions { cNames :: [String]
                                           , enumNames :: [String]
                                           , enumValues :: [String]
                                           }

type BuilderData = [ClassInfo] 

data ClassInfo = ClassInfo { cName        :: String
                           , genericNames :: Maybe (String, String)
                           , mInfo        :: [MethodInfo]
                           , recUsages    :: [(String, UsageImpl)]
                           } deriving Show

data MethodInfo = MethodInfo { mName  :: String
                             , pName  :: String
                             , fNames :: [String]
                             , genericNames' :: Maybe (String, String)
                             } deriving Show

findMethodInfo :: String -> ClassInfo -> Maybe MethodInfo
findMethodInfo name classInfo = 
    let found = filter ((== name) . mName) $ mInfo classInfo 
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
    let fieldNames     = map fieldName $ classFields cls
        methodInfo     = map (createMethodInfo fieldNames genericNames) $ classMethods cls
        name           = className cls
        recUsages      = convertUsageList (classRecUsage cls) genericNames :: [(String, UsageImpl)]
        genericNames   = classGeneric cls
    in ClassInfo name genericNames methodInfo recUsages

createMethodInfo :: [String] -> Maybe (String, String) -> CstMethod  -> MethodInfo
createMethodInfo fields gen method = 
    MethodInfo (methodName method) (parameterName method) fields gen

lookupRecUsages :: [ClassInfo] -> String -> Maybe [(String, UsageImpl)]
lookupRecUsages clsInfo name = -- error "not implemented"
    let found = filter ((name ==) . cName) clsInfo 
    in if (null found)
            then Nothing
            else Just . recUsages $ head (found) 

-- converter
convertEnums :: CstEnum -> EnumDef
convertEnums cstEnum = EnumDef (enumName cstEnum) (enumLabels cstEnum)

convertClass :: GlobalDefinitions -> BuilderData -> CstClass -> Either String Class 
convertClass global classesData cls =  do
    convertedGeneric' <- convertedGeneric
    if not $ null failedFields
        then Left $ "failed to convert fields " ++ concat failedFields
        else if not $ null failedMethods 
                then Left $ "failed to convert methods " ++ concat failedMethods
                else Right $ Class name convertedGeneric' (Usage usage recursiveU) succeededFields succeededMethods
    where 
          usage            = convertUsage (classUsage cls) (classGeneric cls) 
          recursiveU       = convertUsageList (classRecUsage cls) (classGeneric cls) 
          classInfo        = createClassInfo cls
          name             = cName classInfo
          fields           = map (convertField global classesData (classGeneric cls)) (classFields cls)
          classData        = filter ((== className cls). cName) classesData
          convertedMethods = map (convertMethod global classesData (head classData)) (classMethods cls)

          convertedGeneric = convertGenericClass $ classGeneric cls 

          (failedFields, succeededFields)   = partitionEithers fields
          (failedMethods, succeededMethods) = partitionEithers convertedMethods

convertGenericClass :: Maybe (String, String) -> Either String ClassGenericType 
convertGenericClass Nothing                             = Right ClassNoGeneric
convertGenericClass (Just (genClassName, genUsageName)) =
    if genClassName == genUsageName
        then Left "class must have difference generic class and usageName"
        else Right $ ClassGeneric genClassName genUsageName

convertMethod :: GlobalDefinitions -> BuilderData -> ClassInfo -> CstMethod  -> Either String Method 
convertMethod global classesData classInfo method = do
    mType'  <- mType
    mpType' <- mpType
    expr'   <- expr
    Right $ Method mType' mName mpType' mpName expr' 
    where className   = cName classInfo
          mName       = methodName method
          mTypeUsage  = methodTypeUsage method >>= \cstUsage -> Just $ convertUsage cstUsage (genericNames classInfo)
          mType       = convertType global classesData classInfo (methodType method) mTypeUsage 
          mpName      = parameterName method
          mpType      = convertType global classesData classInfo (parameterType method) mpTypeUsage 
          mpTypeUsage  = parameterTypeUsage method  >>= \cstUsage -> Just $ convertUsage cstUsage (genericNames classInfo)

          mInfo       = fromMaybe (Left "unable to find methodInfo") $ Right <$> mName `findMethodInfo` classInfo 
          expr        = mInfo >>= \mInfo' -> convertExpression global classesData mInfo' (methodExpr method)

convertType :: GlobalDefinitions -> BuilderData -> ClassInfo -> CstType -> Maybe UsageImpl -> Either String Type 
convertType global classesData classInfo myType u =
    case myType of
        (CstSimpleType name) | name == "void" && isNothing u -> 
                                        Right $ BType VoidType
                             | name == "bool" && isNothing u ->
                                        Right $ BType BoolType
                             | name `elem` (enumNames global) && isNothing u -> 
                                        Right $ BType $ EnumType name
                             | isJust u -> 
                                        Left $ name ++ "[" ++ show (fromJust u) ++ "] does't make any sense"
        (CstClassType name gen usage) | name `elem` (cNames global) -> 
                                            let u'    = convertUsage usage (genericNames classInfo) :: UsageImpl
                                                gen'  = convertGenericType global classesData (genericNames classInfo) gen
                                                found = filter (\cls' -> cName cls' == name) classesData
                                                recU  = recUsages . head $ found
                                            in if not . null $ found
                                                    then Right $ CType (name, gen', Usage u' recU)
                                                    else Left $ "class not found " ++ show name
                                      | True -> Left $ "error class " ++ name ++ " not found"
                                        
convertGenericType :: GlobalDefinitions -> BuilderData -> Maybe (String, String) -> CstGenInstance -> GenericInstance
convertGenericType global classData genNames CstGenBot                                                    = GenericBot
convertGenericType global classData genNames@(Just (genClassName, genUsageName)) (CstGenInstance n rec u) = 
    let rec' = convertGenericType global classData genNames rec
        u'  = convertUsage u genNames
        recU = filter (\x -> cName x == n) classData 
    in if genClassName == n
            then if rec' == GenericBot && u' == UsageGenericVariable
                        then GenericClass genClassName genUsageName
                        else error "could not find generic class in convertGenericType"
            else if u' /= UsageGenericVariable && not (null recU)
                        then GenericInstance n rec' (Usage u' (recUsages (head recU)))
                        else error "could not find generic class in convertGenericType"
convertGenericType global classData Nothing (CstGenInstance n rec u) = 
     let rec' = convertGenericType global classData Nothing rec
         u'   = convertUsage u Nothing 
         recU = filter (\x -> cName x == n) classData 
    in if u' /= UsageGenericVariable && not (null recU)
            then GenericInstance n rec' (Usage u' (recUsages (head recU))) 
            else error "convertGenericType error generic usage but not generic class"


convertField :: GlobalDefinitions -> BuilderData -> Maybe (String, String) -> CstField -> Either String Field
convertField global classData genNames field = liftA2 Field fieldType' (return (fieldName field))
    where fieldType' = convertFieldType global classData genNames (fieldType field) (fieldGen field) 

convertFieldType :: GlobalDefinitions -> BuilderData -> Maybe (String, String) -> String -> CstGenInstance -> Either String FieldType
convertFieldType global classData genNames name gen = 
    let base'     = BaseFieldType <$> convertBaseType global name
        classType = convertClassTypeField global classData genNames name gen
    in base' <> classType

convertClassTypeField :: GlobalDefinitions -> BuilderData -> Maybe (String, String) -> String -> CstGenInstance -> Either String FieldType
convertClassTypeField global classData (Just (n, _)) name genInstance = 
    if n == name
        then Right $ GenericField
        else convertClassTypeField' global classData name genInstance
convertClassTypeField global classData Nothing name genInstance = 
    convertClassTypeField' global classData name genInstance

convertClassTypeField' ::GlobalDefinitions -> BuilderData -> String -> CstGenInstance -> Either String FieldType
convertClassTypeField' global classData name genInstance 
    | name `elem` (cNames global)           = Right $ ClassFieldGen name (convertGenericType global classData Nothing genInstance)
    | otherwise                             = Left $ "Unknown field " ++ name

convertBaseType :: GlobalDefinitions -> String -> Either String BaseType
convertBaseType global field 
    | field == "void"                 = Right $ VoidType
    | field == "bool"                 = Right $ BoolType
    | field `elem` (enumNames global) = Right $ EnumType field
    | otherwise                       = Left "cannot convert to base type"

updateRecursiveUsage :: [ClassInfo] -> String -> Usage -> Either String Usage
updateRecursiveUsage classes name usage =
    let recursiveUsage = lookupRecUsages classes name <?> ("uable to find recursive usage for " ++ name) 
    in (\rec' -> usage {recursiveUsages = rec'}) <$> recursiveUsage

convertUsage :: CstUsage -> Maybe (String, String)-> UsageImpl
convertUsage (CstUsageChoice xs)  r                 = UsageChoice $ convertUsageList xs r
convertUsage (CstUsageBranch xs)  r                 = UsageBranch $ convertUsageList xs r
convertUsage (CstUsageEnd)        r                 = UsageEnd
convertUsage (CstUsageVariable v) Nothing           = UsageVariable v
convertUsage (CstUsageVariable v) (Just (_, uname)) = 
    if v == uname 
        then UsageGenericVariable
        else UsageVariable v

convertUsageList :: [(String, CstUsage)] -> Maybe (String, String) -> [(String, UsageImpl)]
convertUsageList l rec = map (second (\u -> convertUsage u rec)) l  

convertExpression :: GlobalDefinitions -> BuilderData -> MethodInfo -> CstExpression -> Either String Expression
convertExpression global classesData methodInfo exp = case exp of
    (CstExprNew cls gen)           -> convertNew cls $ convertGenericType global classesData (genericNames' methodInfo) gen 
    (CstExprAssign fieldName expr) -> convertAssign global classesData methodInfo fieldName expr
    (CstExprCall ref method expr)  -> convertCall global classesData methodInfo ref method expr
    (CstExprSeq expr1 expr2)       -> convertSeq global classesData methodInfo expr1 expr2
    (CstExprIf cond texpr fexpr)   -> convertIf global classesData methodInfo cond texpr fexpr 
    (CstExprLabel name expr)       -> convertLabel global classesData methodInfo name expr
    (CstExprContinue name)         -> convertContinue global methodInfo name
    (CstExprBoolConst bool)        -> convertBool global methodInfo bool 
    (CstExprNull)                  -> convertNull
    (CstExprUnit)                  -> convertUnit
    (CstExprSwitch cond matches)   -> convertSwitch global classesData methodInfo cond matches
    (CstExprIdentifier str)        -> convertIdentifier global methodInfo str

convertNew :: String -> GenericInstance -> Either String Expression
convertNew name gen = Right $ ExprNew name gen
  
convertAssign :: GlobalDefinitions -> BuilderData -> MethodInfo -> String -> CstExpression -> Either String Expression
convertAssign global classesInfo methodInfo fieldName expr = 
    ExprAssign fieldName <$> convertExpression global classesInfo methodInfo expr 

convertCall :: GlobalDefinitions -> BuilderData -> MethodInfo -> String -> String -> CstExpression -> Either String Expression
convertCall global classesInfo methodInfo reference method expr = 
    ExprCall <$> ref <*> return method <*> convertExpression global classesInfo methodInfo expr
    where ref = convertReference global methodInfo reference 

convertSeq :: GlobalDefinitions -> BuilderData -> MethodInfo -> CstExpression -> CstExpression -> Either String Expression
convertSeq global classesInfo methodInfo expr1 expr2 = 
    liftA2 ExprSeq expr1' expr2'
    where expr1' = convertExpression global classesInfo methodInfo expr1
          expr2' = convertExpression global classesInfo methodInfo expr2

convertIf :: GlobalDefinitions -> BuilderData -> MethodInfo -> CstExpression -> CstExpression -> CstExpression -> Either String Expression
convertIf global classesInfo methodInfo cond texpr fexpr = 
    liftA3 ExprIf cond' texpr' fexpr'
    where conv   = convertExpression global classesInfo methodInfo
          cond'  = conv cond
          texpr' = conv texpr
          fexpr' = conv fexpr 

convertLabel :: GlobalDefinitions -> BuilderData -> MethodInfo -> String -> CstExpression -> Either String Expression
convertLabel global classesInfo methodInfo name expr =
    ExprLabel name <$> convertExpression global classesInfo methodInfo expr

convertContinue :: GlobalDefinitions -> MethodInfo -> String -> Either String Expression
convertContinue global methodInfo name = Right $ ExprContinue name

convertBool :: GlobalDefinitions -> MethodInfo -> Bool -> Either String Expression
convertBool global methodInfo = Right . ExprBoolConst

convertNull :: Either String Expression
convertNull = Right ExprNull

convertUnit :: Either String Expression
convertUnit = Right ExprUnit

convertSwitch :: GlobalDefinitions -> BuilderData -> MethodInfo -> CstExpression -> [(String, CstExpression)] -> Either String Expression
convertSwitch global classesInfo methodInfo expr matches =  
    do call' <- call
       case call' of 
            (ExprCall ref' _ expr') -> ExprSwitch ref' call' <$> matches'
            _                       -> Left $ "failed to convert call"
    where (CstExprCall ref method param) = expr

          call      = convertCall global classesInfo methodInfo ref method param

          matches'' = sequence $ map (uncurry (convertLabel global classesInfo methodInfo)) matches
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
                        
