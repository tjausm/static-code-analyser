module MFP where
import AttributeGrammar
import qualified Data.Map as M
import Data.Maybe (fromMaybe)



type L a =  a -> a -> a -- Lattice Join
type FancyF a = a -- Transfer functions TODO: add functions
type Labels = [Int]
data F = MkFlow FlowDir [Flow]
data FlowDir = Backward | Forward deriving Eq-- flow direction 
type E = [Int] -- extremal labels
type J a = a -- extremal value
type Bottom a = a
type LambdaF a = M.Map Int (a -> a) -- mapping labels to transfer functions

-- TODO: should bottom be passed as an argument?
-- Should a have both Eq and Ord typeclass since it should be a partial order
maximalFixedPoint :: Ord a => Labels -> Bottom a -> L a -> FancyF a -> F -> E -> J a -> LambdaF a ->  [a]
maximalFixedPoint labels bottom join fancyF f e j lambF =
    -- Step 1
    let w = f
        analysis = map (\label -> if  label `elem` e then j else bottom) labels -- labels set extremal labels to jota
    -- Step 2
    in step2 w lambF analysis join

step2 :: Ord a => F -> LambdaF a -> [a] -> L a ->  [a]
step2 (MkFlow _ []) lambF analysis join = analysis -- if W == Nil return analysis
step2 (MkFlow dir (w:ws)) lambF analysis join =
    let l = if dir == Forward then fstLabel w else sndLabel w
        l' = if dir == Backward then sndLabel w else fstLabel w
        fl = fromMaybe id (M.lookup l lambF) -- get lambda function for label l 
        analysis' = replacel l (analysis!!l' `join` fl (analysis!!l)) analysis -- update l'
        w' = filter ((l' ==) . fstLabel) (w:ws) -- get all flow tupples of the form (l', _)
    in if fl (analysis!!l) > (analysis!!l') -- check if transfer function over l > l'
        then step2 (MkFlow dir w') lambF analysis' join
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
