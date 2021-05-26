module MFP where
import AttributeGrammar
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data L a =  MkLattice (a -> a -> a) (Bottom a) -- Lattice Join
type FancyF a = a -- Transfer functions TODO: add functions
data F = MkFlow FlowDir [Flow]
type E = [Int] -- extremal labels
type J a = a -- extremal value
type LambdaF a = M.Map Int (a -> a) -- mapping labels to transfer functions

type Labels = [Int]
type Bottom a = a
data FlowDir = Backward | Forward deriving Eq-- flow direction 
-- TODO: should bottom be passed as an argument?
-- Should a have both Eq and Ord typeclass since it should be a partial order
maximalFixedPoint :: Ord a => L a -> FancyF a -> F -> E -> J a -> LambdaF a ->  Labels ->  [a]
maximalFixedPoint lattice@(MkLattice _ bottom) fancyF f e j lambF labels =
    -- Step 1
    let w = f
        analysis = map (\label -> if  label `elem` e then j else bottom) labels -- labels set extremal labels to jota
    -- Step 2
    in step2 lattice w lambF analysis 

step2 :: Ord a =>  L a -> F -> LambdaF a -> [a] ->  [a]
step2 _ (MkFlow _ []) _ analysis = analysis -- if W == Nil return analysis
step2 lattice@(MkLattice join bottom) (MkFlow dir (w:ws)) lambF analysis  =
    let l = if dir == Forward then fstLabel w else sndLabel w
        l' = if dir == Backward then sndLabel w else fstLabel w
        fl = fromMaybe id (M.lookup l lambF) -- get lambda function for label l 
        analysis' = replacel l (analysis!!l' `join` fl (analysis!!l)) analysis -- update l'
        w' = filter ((l' ==) . fstLabel) (w:ws) -- get all flow tupples of the form (l', _)
    in if fl (analysis!!l) > (analysis!!l') -- check if transfer function over l > l'
        then step2 lattice (MkFlow dir w') lambF analysis' 
        else analysis

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
