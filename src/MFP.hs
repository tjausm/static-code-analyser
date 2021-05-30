module MFP where
import AttributeGrammar
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Debug.Trace

type Delta = [Int] 

data L a =  MkLattice (a -> a -> a) (Bottom a) -- Lattice Join
type EmbellishedL a = M.Map Delta [a] -- Not sure about the list yet.  
data F = MkFlow FlowDir [Flow]
type IF = [(Int, Int, Int, Int)]
type E = [Int] -- extremal labels
type J a = a -- extremal value
type LambdaF a = Int -> a -> a -- mapping labels to transfer functions

type Bottom a = a
data FlowDir = Backward | Forward deriving Eq-- flow direction 


-- Should a have both Eq and Ord typeclass since it should be a partial order
maximalFixedPoint :: Show a => Ord a => L a -> F -> IF -> Int -> E -> J a -> LambdaF a -> [(a,a)]
maximalFixedPoint lattice@(MkLattice _ bottom)  flow@(MkFlow _ w) interf k e j lambF  =
    -- Step 1
    let labels = genLabels w
        analysis = map (\label -> if  label `elem` e then j else bottom) labels -- labels set extremal labels to jota
    -- Step 2
    in  step3 lambF flow $
        step2 lattice flow interf k w [] lambF analysis

step2 :: Show a => Ord a => L a -> F -> IF -> Int -> [Flow] -> Delta -> LambdaF a -> [a] -> [a]
step2 _ _ _ _ [] _ _ analysis = analysis                                                                -- if W == Nil return analysis
step2 lattice@(MkLattice join bottom) flow@(MkFlow dir f) interf k (w:ws) delta lambF analysis  =
    let l = if dir == Forward then fstLabel w else sndLabel w                                         -- lower 1 index because haskell lists start at 0
        l' = if dir == Forward then sndLabel w  else fstLabel w
        fl = lambF l                                                                                  -- get lambda function for label l 
        analysis' = replacel l' (analysis!!(l'-1) `join` fl (analysis!!(l-1))) analysis               -- update l'
        w' = if dir == Forward then filter ((l' ==) . fstLabel) f else filter ((l' ==) . sndLabel) f  -- get all flow tupples of the form (l', _) from the flow
        calls = foldr (\x y -> [(getFst x)] ++ y ) [] interf
        returns = foldr (\x y -> [(getThird x)] ++ y ) [] interf

        normal = if fl (analysis!!(l-1)) > analysis!!(l'-1)                     -- check if transfer function over l > l'
                    then step2 lattice flow interf k (w' ++ ws) delta lambF analysis'   -- recurse with updated analysis (maybe we can just recurse anyhow with updated value?)
                    else step2 lattice flow interf k ws delta lambF analysis            -- recurse without updated analysis

        analysisc = replacel l' (multiJoin (map (\x -> analysis!!(x-1)) (getCallLabels interf l')) join bottom) analysis  -- fl moet hier nog in verwerkt worden + in deze call hieronder. 

        delta' = updateDelta delta l k
        deltar = updateDelta delta (-1) k 
    in
        if elem l calls 
            then if fl (multiJoin (map (\x -> analysis!!(x-1)) (getCallLabels interf l')) join bottom) > analysis!!(l'-1)        --join anlysis over all entry options.     -- context switch --> update delta value
                    then step2 lattice flow interf k (w' ++ ws) delta' lambF analysisc                               -- still need to get the correct analysis via delta. 
                    else step2 lattice flow interf k ws delta' lambF analysis        
            else if elem l returns  -- or do we need to check l' is returnlabel instead of l is exit label?
                then if fl (analysis!!(l-1)) > analysis!!(l'-1)                             -- another context switch --> update delta value
                    then step2 lattice flow interf k (w' ++ ws) deltar lambF analysis'   
                    else step2 lattice flow interf k ws deltar lambF analysis       
                else normal                                                                 -- normal continuation 
        --if  logStep analysis (w:ws) w' l l' calls returns fl $                 -- Uncomment to trace algorithm
        --    fl (analysis!!(l-1)) > analysis!!(l'-1)                    -- check if transfer function over l > l'
        --then step2 lattice flow interf (w' ++ ws) lambF analysis'      -- recurse with updated analysis (maybe we can just recurse anyhow with updated value?)
        --else step2 lattice flow interf ws lambF analysis               -- recurse without updated analysis

-- return result as tuples of (entry,exit) value
step3 :: LambdaF a -> F -> [a] -> [(a,a)]
step3 lambF (MkFlow dir f) analysis = if dir == Forward then zip analysis analysis' else zip analysis' analysis
    where
        analysis' = zipWith (\ f a -> f a) fl analysis
        fl = [lambF i | i <- [1 .. (length analysis)]]

updateDelta :: Delta -> Int -> Int -> Delta
updateDelta d (-1) _ = tail d 
updateDelta d l  k = l : take k d

multiJoin :: [a] -> (a -> a -> a) -> a -> a
multiJoin as join bottom = foldr join bottom as

logStep :: (Show a1, Ord a1) => [a1] -> [Flow] -> [Flow] -> Int -> Int -> [Int] -> [Int] -> (a1 -> a1) -> a2 -> a2
logStep analysis (w:ws) w' l l' calls returns fl = trace ("\n\nanalysis = " ++ show analysis)
            trace ("step2: (w:ws) = " ++ unwords (map show (w:ws)))
            trace ("step2: w' = " ++ unlines (map show w'))
            trace ("step2: l = " ++ show l  ++ ", l' = " ++ show l' )
            trace ("step2: calls = " ++ show calls )
            trace ("step2: returns = " ++ show returns )
            trace ("step2: if " ++  show (fl (analysis!!(l-1))) ++ ">" ++ show (analysis!!(l'-1)) ++ " = " ++ show (fl (analysis!!(l-1)) > analysis!!(l'-1)))

-- Replaces l'th element in a list
replacel :: Int -> a -> [a] -> [a]
replacel l l' w = lhs ++ [l'] ++ rhs
    where
        lhs = take (l - 1) w
        rhs = drop l w

-- Functions to easily process Flow type
instance Show Flow where
    show (Inter (a,b)) = show "( " ++ show a ++ show  ";" ++ show b ++ show ")"
    show (Intra (a,b)) = show "( " ++ show a ++ show  "," ++ show b ++ show ")"
fstLabel :: Flow -> Int
fstLabel (Intra f) = fst f
fstLabel (Inter f) = fst f
sndLabel :: Flow -> Int
sndLabel (Inter f) = snd f
sndLabel (Intra f) = snd f
getFlow :: F -> [Flow]
getFlow (MkFlow _ f) = f
getDir :: F -> FlowDir
getDir (MkFlow d _) = d

getFst :: (Int, Int, Int, Int) -> Int
getFst (a, _, _, _) = a
getSnd :: (Int, Int, Int, Int) -> Int
getSnd (_, a, _, _) = a
getThird :: (Int, Int, Int, Int) -> Int
getThird (_, _, a, _) = a

getCallLabels :: [(Int, Int, Int, Int)] -> Int -> [Int]
getCallLabels []     _ = []
getCallLabels (i:is) l = case getSnd i of
                         l -> getFst i : getCallLabels is l 
                         _ -> getCallLabels is l

-- Gets highest label m in the flow list and gens a list [1..m] 
genLabels :: [Flow] -> [Int]
genLabels flow = [1..maxLab]
  where
    maxLab = foldr f 1 flow
    f a b = max b (max (fstLabel a) (sndLabel a))

-- show algo result as list of labels and results using given show function (seperated by newlines)
showMFP :: (a -> String) -> [(a,a)] -> String
showMFP showPoint mfp = unlines (mergeList labels sMfp)
    where
        labels = [show i | i <- [1.. (length sMfp)]]
        sMfp = map  (\(x,y) -> "Entry: \n" ++ showPoint x ++ "\nExit: \n" ++ showPoint y) mfp

-- intersperse 2 lists [a,b] -> [1,2] -> [a,1,b,2]
mergeList::[a]->[a]->[a]
mergeList [] [] = []
mergeList  _ [] = []
mergeList [] _  = []
mergeList (x:xs) (y:ys) = x:y:mergeList xs ys
