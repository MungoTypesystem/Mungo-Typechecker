module SanityCheck where

import Data.List (group, sort, nub, groupBy)
import AST 
import MungoParser
import Data.Maybe
import Data.Graph
import Debug.Trace (trace)

-- Helper functions 
duplicates :: [String] -> [String]
duplicates = map head . filter ((> 1) . length) . group

getAllTypes :: [CstClass] -> [CstEnum] -> [String]
getAllTypes classes enums = 
    ["void", "bool"]
    ++ classNames
    ++ enumNames
    where
        classNames = map className classes
        enumNames  = map enumName enums

-- PROGRAM CHECKING
    -- Valid field types
    -- Valid parameter types
    -- Valid return types
    -- Duplicate class and enum names
    -- Sanity check class
    -- Unique literals
sanityCheck :: [CstProgram] -> [String]
sanityCheck programs = 
    filter (not . null) $ (sanityCheckEnums programs)
                           ++ (sanityCheckPrograms programs)

sanityCheckDuplicateNames :: [CstClass] -> [CstEnum] -> [String]
sanityCheckDuplicateNames classes enums =
    map (++ " duplicated class/enum name") (duplicates names)
    where
        classNames = sort $ map className classes
        enumNames  = sort $ map enumName enums
        names      = classNames ++ enumNames

sanityCheckPrograms :: [CstProgram] -> [String]
sanityCheckPrograms programs =
    duplicateNameErrs ++ fieldTypeErrs ++ methodTypeErrs ++ classErrs ++ overlapErrs
    where
        classes           = concat $ map progClasses programs
        enums             = concat $ map progEnums programs
        supportedTypes    = getAllTypes classes enums
        duplicateNameErrs = sanityCheckDuplicateNames classes enums
        fieldTypeErrs     = concat $ map (`sanityCheckFieldTypes` supportedTypes) $ map classFields classes
        methodTypeErrs    = concat $ map (`sanityCheckMethodTypes` supportedTypes) $ map classMethods classes
        overlapErrs       = map (`sanityCheckClassGenPar` supportedTypes) $ classes
        classErrs         = concat $ map sanityCheckClass classes

--- Check Types with generics
    -- Check type names exist
    -- Check the type names of type variables
    -- Check recursive variables of a generic usage does not overlap with 
    --   the recusive variables of the class declaration.
typeNameFromCstType :: CstType -> String
typeNameFromCstType t = case t of
                            (CstSimpleType n) -> n
                            (CstClassType n _ _) -> n

typeNameFromCstGenInstance :: CstGenInstance -> String
typeNameFromCstGenInstance gen = 
    case gen of
        (CstGenInstance n _ _) -> n
        (CstGenBot) -> "//Bot"

cstGenFromCstType :: CstType -> CstGenInstance
cstGenFromCstType t =
    case t of
        (CstSimpleType n) -> CstGenBot
        (CstClassType _ gen _) -> gen

getAllGenTypes :: CstGenInstance -> [String]
getAllGenTypes gen =
    case gen of
        (CstGenInstance n gen' _) -> [n] ++ getAllGenTypes gen'
        (CstGenBot) -> []

sanityCheckTypeVariables :: [CstGenInstance] -> [String] -> [String] -> String -> [String]
sanityCheckTypeVariables (g:gs) (n:ns) supportedTypes msg =
    sanityCheckTypeNames allGenTypes [n] supportedTypes msg
    ++ sanityCheckTypeVariables gs ns supportedTypes msg
    where
        allGenTypes = getAllGenTypes g

sanityCheckTypeVariables _ _ supportedTypes msg = []

sanityCheckTypeNames :: [String] -> [String] -> [String] -> String -> [String]
sanityCheckTypeNames (ut:uts) (n:ns) supportedTypes msg =
    if (ut == "//Bot") || (ut `elem` supportedTypes)
        then sanityCheckTypeNames uts ns supportedTypes msg
        else [(errMsg ut n msg)]
             ++ sanityCheckTypeNames uts ns supportedTypes msg--}
sanityCheckTypeNames _ _ supportedTypeNames errMsg = []

-- UsedTypes -> Identifier -> SupportedTypes -> ErrorMsg
sanityCheckTypes :: [CstType] -> [String] -> [String] -> String -> [String]
sanityCheckTypes uts ns supportedTypes msg =
    sanityCheckTypeNames usedTypeNames ns supportedTypes msg
    ++ sanityCheckTypeVariables typeVariables ns supportedTypes msg
    where
        usedTypeNames      = map typeNameFromCstType uts
        typeVariables      = map cstGenFromCstType uts

errMsg :: String -> String -> String -> String
errMsg identifierType identifier identifierMsg = 
    "Unsupported type " ++ identifierType ++ " "
    ++ identifierMsg ++ " " ++ identifier

sanityCheckFieldTypes :: [CstField] -> [String] -> [String]
sanityCheckFieldTypes fields types =
    sanityCheckTypes usedTypes fieldNames types msg
    where
        usedTypes  = map toCstType $ zip (map fieldType fields) (map fieldGen fields)
        toCstType  = (\(tt, gt) -> (CstClassType tt gt CstUsageEnd))
        fieldNames = map fieldName fields
        msg        = "used in field"

sanityCheckMethodTypes :: [CstMethod] -> [String] -> [String]
sanityCheckMethodTypes methods types = 
    (sanityCheckTypes methodTypes methodNames types methodMsg)
    ++ (sanityCheckTypes parameterTypes parameterNames types parameterMsg)
    where
        methodTypes    = map methodType methods
        methodNames    = map methodName methods
        methodMsg      = "used in return of method"
        parameterTypes = map parameterType methods
        parameterNames = map parameterName methods
        parameterMsg   = "used in parameter"

getGenParameters :: CstGenInstance -> Maybe (CstGenInstance, CstUsage)
getGenParameters t =
    case t of
        (CstGenBot) -> Nothing
        (CstGenInstance n genRec usage) -> Just (genRec, usage)

-- Check that alpha and beta does not overlap with any declared classes.
sanityCheckClassGenPar :: CstClass -> [String] -> String
sanityCheckClassGenPar cls supportedTypes = 
    if isNothing genericPar
        then []
    else checkOverlap (fromJust genericPar) supportedTypes $ className cls
    where
        genericPar = classGeneric cls


checkOverlap :: (String, String) -> [String] -> String -> String
checkOverlap genPar supportedTypes clsName =
    if alpha `elem` supportedTypes || beta `elem` supportedTypes
        then "Overlap error in generic alpha or beta in class " ++ clsName
    else []
    where
        (alpha, beta) = genPar

-- Enum CHECKING
    -- Unique labels
sanityCheckEnums :: [CstProgram] -> [String]
sanityCheckEnums programs = 
    map (++ " duplicated label") (duplicates labels)
    where 
        labels = sort $ concat $ map enumLabels $ concat $ map progEnums programs

-- CLASS CHECKING
-- Combines errors from sub-class checkers.
sanityCheckClass :: CstClass -> [String]
sanityCheckClass cls@(CstClass name _ usage recusage fields methods) =
    usageErrs ++ fieldErrs ++ methodErrs ++ parameterErrs
    where 
        usageErrs      = sanityCheckUsageRecVariables usage recusage
                        ++ sanityCheckUsageMethodExist usage recusage methods
                        ++ [sanityCheckUsageGoesToEnd cls]
        fieldErrs      = sanityCheckFieldVariables fields
        methodErrs     = sanityCheckMethods methods
        parameterErrs  = sanityCheckParameters methods fields 

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
        parameterNames = sort $ (nub $ map fieldName fields) ++ (nub (map parameterName methods))

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
        methodNames      = map methodName methods
        usageMethodNames = usageMethods usage
        recUsageMethods = concat $ map usageMethods $ map snd recUsage

-- GRAPH STUFF for checking that usage goes to end.

type Vertices = [String]
type Edges = [(String, String)]

getNextMethods :: String -> CstUsage -> [String]
getNextMethods context usage =
    case usage of
        (CstUsageEnd) -> ["end"]
        (CstUsageVariable var) -> [var]
        (CstUsageChoice choices) -> concat $ map (getNextMethods context) $ map snd choices
        (CstUsageBranch branchs) -> map (++ (context ++ "//")) $ map fst branchs

buildRecUsagePaths :: [(String, CstUsage)] -> Vertices -> Edges -> (Vertices, Edges)
buildRecUsagePaths (r:rs) vertices edges =
    buildRecUsagePaths rs nnvertices nnedges
    where
        (rec, usage) = r
        nextMethods = getNextMethods rec usage
        nedges = edges ++ (zip (replicate (length nextMethods) rec) nextMethods)
        nvertices = vertices ++ [rec]
        (nnvertices, nnedges) = buildUsagePaths usage nvertices nedges rec
buildRecUsagePaths _ vertices edges = (vertices, edges)

buildUsagePathsBranch :: [(String, CstUsage)] -> Vertices -> Edges -> String -> (Vertices, Edges)
buildUsagePathsBranch (u:us) vertices edges context = 
    buildUsagePathsBranch us n2vertices n2edges context
    where
        (m, usage) = u
        currM = m ++ context ++ "//"
        nvertices = vertices ++ [currM]
        nextMethods = getNextMethods context usage
        nedges = edges ++ (zip (replicate (length nextMethods) currM) nextMethods)
        (n2vertices, n2edges) = buildUsagePaths usage nvertices nedges context
buildUsagePathsBranch _ vertices edges context = (vertices, edges)

buildUsagePathsChoice :: [CstUsage] -> Vertices -> Edges -> String -> (Vertices, Edges)
buildUsagePathsChoice (u:us) vertices edges context =
    buildUsagePathsChoice us nvertices nedges context
    where
        (nvertices, nedges) = buildUsagePaths u vertices edges context

buildUsagePaths :: CstUsage -> Vertices -> Edges -> String -> (Vertices, Edges)
buildUsagePaths usage vertices edges context = 
    case usage of
        (CstUsageEnd) -> (vertices ++ ["end"], edges)
        (CstUsageVariable var) -> (vertices ++ [var], edges)
        (CstUsageChoice choices) -> buildUsagePathsChoice (map snd choices) vertices edges context
        (CstUsageBranch branchs) -> buildUsagePathsBranch branchs vertices edges context

buildUsageGraph :: CstUsage -> [(String, CstUsage)] -> (Vertices, Edges)
buildUsageGraph usage recusage = 
    (nvertices, nedges)
    where
        (vertices, edges) = buildUsagePaths usage [] [] ""
        (nvertices, nedges) = buildRecUsagePaths recusage vertices edges

bfs :: String -> Vertices -> Edges -> Vertices -> Vertices
bfs s queue@(q:qs) edges visited =
    bfs s' queue' edges nvisited
    where
        nvisited = visited ++ [s]
        fromS = filter (\e -> s == (fst e)) edges
        toVertices = filter (\x -> x `notElem` nvisited && x `notElem` queue) $ map snd fromS
        queue' = qs ++ toVertices
        s' = q
bfs s _ edges visited = 
    if (s `elem` visited) then visited
    else bfs s [s] edges visited

graphBfs :: Vertices -> Edges -> [Vertices]
graphBfs (v:vs) edges =
    [visitedFromV] ++ graphBfs vs edges
    where
        visitedFromV = bfs v [] edges []
graphBfs _ edges = []

sanityCheckUsageGoesToEnd :: CstClass -> String
sanityCheckUsageGoesToEnd (CstClass name _ usage recUsage _ _) =
    if (length notEnd) > 0 
        then "Usage does not go to end in class " ++ name
    else []
    where
        (vertices, edges) = buildUsageGraph usage recUsage
        allPaths = graphBfs vertices edges
        notEnd = filter (\p -> "end" `notElem` p) allPaths

viewGraph' :: [CstProgram] -> IO()
viewGraph' programs = do
    let program = head programs
    let cls = head $ progClasses program
    let usage = classUsage cls
    let recUsage = classRecUsage cls
    let (vertices, edges) = buildUsageGraph usage recUsage
    print vertices
    print edges
    print vertices
    print edges