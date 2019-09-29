module SanityCheck where

import Data.List (group, sort)
import AST 
import MungoParser 

sanityCheck :: [CstClass] -> [([String], Maybe Class)]
sanityCheck classes = map sanityCheck' classes

sanityCheck' :: CstClass -> ([String], Maybe Class)
sanityCheck' (CstClass name usage recusage fields methods) =
   ([""], Nothing) 

-- helper functions 
duplicates :: [String] -> [String]
duplicates = map head . filter ((> 1) . length) . group

-- CLASS checking

classNames :: [CstClass] -> [String]
classNames = map className
    where className :: CstClass -> String
          className (CstClass name _ _ _ _) = name

-- FIELD CHECKING


-- list of errors
sanityCheckUsageRecVariables :: CstUsage -> [(String, CstUsage)] -> [String]
sanityCheckUsageRecVariables usage recUsage =
    map (++ " duplicated usage variable ") (duplicates usageNames)
    where usageNames = sort $ map fst recUsage

