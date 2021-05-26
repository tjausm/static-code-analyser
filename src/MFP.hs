module MFP where
import AttributeGrammar
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Debug.Trace

data L a =  MkLattice (a -> a -> a) (Bottom a) -- Lattice Join
type FancyF a = a -- Transfer functions TODO: add functions
data F = MkFlow FlowDir [Flow]
type E = [Int] -- extremal labels
type J a = a -- extremal value
type LambdaF a = Int -> a -> a -- mapping labels to transfer functions

type Labels = [Int]
type Bottom a = a
data FlowDir = Backward | Forward deriving Eq-- flow direction 
-- TODO: should bottom be passed as an argument?
-- Should a have both Eq and Ord typeclass since it should be a partial order
maximalFixedPoint :: Show a => Ord a => L a -> FancyF a -> F -> E -> J a -> LambdaF a ->  Labels ->  [(a,a)]
maximalFixedPoint lattice@(MkLattice _ bottom) fancyF f@(MkFlow _ flow) e j lambF labels =
    -- Step 1
    let labels = genLabels flow
        w = f
        analysis = map (\label -> if  label `elem` e then j else bottom) labels -- labels set extremal labels to jota
    -- Step 2
    in trace ("analysis = " ++ show analysis) $ 
        step3 lambF $ 
        step2 lattice w flow lambF analysis

step2 :: Show a => Ord a =>  L a -> F -> [Flow] -> LambdaF a -> [a] ->  [a]
step2 _ (MkFlow _ []) _ _ analysis = analysis                                                 -- if W == Nil return analysis
step2 lattice@(MkLattice join bottom) (MkFlow dir f) (w:ws) lambF analysis  =
    let l = if dir == Forward then fstLabel w else sndLabel w                        -- lower 1 index because haskell lists start at 0
        l' = if dir == Forward then sndLabel w  else fstLabel w 
        fl = lambF l                                                                        -- get lambda function for label l 
        analysis' = replacel (l'-1) (analysis!!(l'-1) `join` fl (analysis!!(l-1))) analysis              -- update l'
        w' = if dir == Forward then filter ((l' ==) . fstLabel) ws else filter ((l' ==) . sndLabel) ws  -- get all flow tupples of the form (l', _)
    in
        if  trace ("step2: (w:ws) = " ++ unlines (map showFlow (w:ws))) 
            trace ("step2: w' = " ++ unlines (map showFlow w')) 
            trace ("step2: l = " ++ show l  ++ ", l' = " ++ show l' )
            trace ("step2: if " ++ map show analysis!!(l-1) ++ ">" ++ map show analysis!!(l'-1)) $

            fl (analysis!!(l-1)) > analysis!!(l'-1)  -- check if transfer function over l > l'
        then step2 lattice (MkFlow dir (w' ++ ws)) f lambF analysis' -- recurse with updated analysis (maybe we can just recurse anyhow with updated value?)
        else step2 lattice (MkFlow dir ws) f lambF analysis -- recurse without updated analysis

-- return result as tupples of entry and exit value
step3 :: LambdaF a -> [a] -> [(a,a)]
step3 lambF analysis = zip analysis analysis'
    where
        analysis' = map (\(f,a) -> f a) (zip fl analysis)
        fl = map lambF [i | i <- [1..(length  analysis)]]

replacel :: Int -> a -> [a] -> [a]
replacel l l' w = lhs ++ [l'] ++ rhs
    where
        lhs = take (l - 1) w
        rhs = drop l w

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

-- Gets highest label m and gens a list [1..m] 
genLabels :: [Flow] -> [Int]
genLabels flow = [1..maxLab]
  where
    maxLab = foldr f 1 flow
    f a b = max b (max (fstLabel a) (sndLabel a))

showFlow :: Flow -> String
showFlow(Inter f) = show f
showFlow(Intra f) = show f

-- show algo result as list of labels and results using given show function (seperated by newlines)
showMFP :: (a -> String) -> [(a,a)] -> String
showMFP showPoint mfp = unlines (mergeList labels sMfp)
    where
        labels = [show i | i <- [1.. (length sMfp)]]
        sMfp = map  (\(x,y) -> "Entry: \n" ++ showPoint x ++ "Exit: \n" ++ showPoint y) mfp

-- intersperse 2 lists [a,b] -> [1,2] -> [a,1,b,2]
mergeList::[a]->[a]->[a]
mergeList [] [] = []
mergeList  _ [] = []
mergeList [] _  = []
mergeList (x:xs) (y:ys) = x:y:mergeList xs ys
