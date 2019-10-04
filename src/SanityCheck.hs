module SanityCheck where

import Data.List (group, sort, nub)
import AST 
import MungoParser

-- Helper functions 
duplicates :: [String] -> [String]
duplicates = map head . filter ((> 1) . length) . group

getAllTypes :: [CstClass] -> [String]
getAllTypes classes = (map className classes) 
                    ++ ["void", "bool"] 

-- PROGRAM CHECKING
    -- Valid field types
    -- Valid parameter types
    -- Valid return types
    -- Duplicate class names
    -- Sanity check class
sanityCheck :: [CstClass] -> [String]
sanityCheck classes =
    classNameErrs ++ fieldTypeErrs ++ methodTypeErrs ++ classErrs
    where
        supportedTypes = getAllTypes classes
        classNameErrs = sanityCheckClassNames classes
        fieldTypeErrs = concat $ map (`sanityCheckFieldTypes` supportedTypes) $ map classFields classes
        methodTypeErrs = concat $ map (`sanityCheckMethodTypes` supportedTypes) $ map classMethods classes
        classErrs = concat $ map sanityCheckClass classes

sanityCheckClassNames :: [CstClass] -> [String]
sanityCheckClassNames classes =
    map (++ " duplicated class name") (duplicates classNames)
    where
        classNames = sort $ map className classes

-- UsedTypes -> Identifier -> SupportedTypes -> ErrorMsg
sanityCheckTypes :: [String] -> [String] -> [String] -> String -> [String]
sanityCheckTypes (ut:uts) (n:ns) supportedTypes msg =
    if ut `elem` supportedTypes
        then sanityCheckTypes uts ns supportedTypes msg
        else [(errMsg ut n msg)]
             ++ sanityCheckTypes uts ns supportedTypes msg
sanityCheckTypes _ _ supportedTypes errMsg = []

errMsg :: String -> String -> String -> String
errMsg identifierType identifier identifierMsg = 
    "Unsupported type " ++ identifierType ++ " "
    ++ identifierMsg ++ " " ++ identifier

sanityCheckFieldTypes :: [CstField] -> [String] -> [String]
sanityCheckFieldTypes fields types =
    sanityCheckTypes usedTypes fieldNames types msg
    where
        usedTypes = map fieldType fields
        fieldNames = map fieldName fields
        msg = "used in field"

sanityCheckMethodTypes :: [CstMethod] -> [String] -> [String]
sanityCheckMethodTypes methods types = 
    (sanityCheckTypes methodTypes methodNames types methodMsg)
    ++ (sanityCheckTypes parameterTypes parameterNames types parameterMsg)
    where
        methodTypes = map methodType methods
        methodNames = map methodName methods
        methodMsg = "used in return of method"
        parameterTypes = map parameterType methods
        parameterNames = map parameterName methods
        parameterMsg = "used in parameter"

-- CLASS checking
-- Combines errors from sub-class checkers.
sanityCheckClass :: CstClass -> [String]
sanityCheckClass (CstClass name usage recusage fields methods) =
    usageErrs ++ fieldErrs ++ methodErrs ++ parameterErrs
    where 
        usageErrs     = sanityCheckUsageRecVariables usage recusage
                        ++ sanityCheckUsageMethodExist usage recusage methods
                        ++ sanityCheckUsageGoesToEnd usage recusage name
        fieldErrs     = sanityCheckFieldVariables fields
        methodErrs    = sanityCheckMethods methods
        parameterErrs = sanityCheckParameters methods fields

-- FIELD CHECKING
    -- Duplicate fields names
sanityCheckFieldVariables :: [CstField] -> [String]
sanityCheckFieldVariables fields =
    map (++ " duplicated field variable name") (duplicates variableNames)
    where
        variableNames = sort $ map fieldName fields

-- METHOD CHECKING
    -- Duplicate method names
    -- Parameter names not the same as fields
sanityCheckMethods :: [CstMethod] -> [String]
sanityCheckMethods methods =
    map (++ " duplicated method name") (duplicates methodNames)
    where
        methodNames = sort $ map methodName methods

sanityCheckParameters :: [CstMethod] -> [CstField] -> [String]
sanityCheckParameters methods fields = 
    map (++ " duplicated parameter name") (duplicates parameterNames)
    where
        parameterNames = sort $ (nub $ map fieldName fields) ++ (map parameterName methods)

-- USAGE CHECKING
    -- Duplicate recursive usages
    -- Methods exist
    -- Usages always go to end
sanityCheckUsageRecVariables :: CstUsage -> [(String, CstUsage)] -> [String]
sanityCheckUsageRecVariables usage recUsage =
    map (++ " duplicated usage variable ") (duplicates usageNames)
    where 
        usageNames = sort $ map fst recUsage

usageMethods :: CstUsage -> [String]
usageMethods CstUsageEnd = []
usageMethods (CstUsageBranch branch) = (map fst branch) ++ (concat (map usageMethods $ map snd branch))
usageMethods (CstUsageChoice choice) = concat $ map usageMethods $ map snd choice
usageMethods _ = []

sanityCheckUsageMethodExist :: CstUsage -> [(String, CstUsage)] -> [CstMethod] -> [String]
sanityCheckUsageMethodExist usage recUsage methods = 
    map (++ " method undefined in usage") $ filter (`notElem` methodNames) $ nub $ recUsageMethods ++ usageMethodNames
    where
        methodNames = map methodName methods
        usageMethodNames = usageMethods usage
        recUsageMethods = concat $ map usageMethods $ map snd recUsage

sanityCheckUsageGoesToEnd :: CstUsage -> [(String, CstUsage)] -> String -> [String]
sanityCheckUsageGoesToEnd usage recusage className = 
   map (++ className) $ sanityCheckUsageGoesToEnd' usage

sanityCheckUsageGoesToEnd' :: CstUsage -> [String]
sanityCheckUsageGoesToEnd' (CstUsageBranch usage) = concat $ map sanityCheckUsageGoesToEnd' $ map snd usage
sanityCheckUsageGoesToEnd' (CstUsageChoice usage) = concat $ map sanityCheckUsageGoesToEnd' $ map snd usage
sanityCheckUsageGoesToEnd' CstUsageEnd = []
sanityCheckUsageGoesToEnd' _ = ["Usage does not go to end in class "]