module MFP where
import AttributeGrammar

type L a =  a -> a -> a -- Lattice Join
type FancyF a = a -- Transfer functions TODO: add functions
type Labels = [Int]
data F = MkFlow FlowDir [Flow]
data FlowDir = Backward | Forward -- flow direction 
type E = [Int] -- extremal labels
type J a = a -- extremal value
type Bottom a = a
type LambdaF a = a -> (a -> a) -- mapping labels to transfer functions




-- TODO: should bottom be passed as an argument?
-- Should a have both Eq and Ord typeclass since it should be a partial order
maximalFixedPoint :: Ord a => Labels -> Bottom a -> L a -> FancyF a -> F -> E -> J a -> LambdaF a -> a
maximalFixedPoint labels bottom l fancyF f e j lambF =
    -- Step 1
    let w = f
        analysis = map (\label -> if  label `elem` e then j else bottom) labels -- labels set extremal labels to jota
    -- Step 2
    in undefined -- step2 w f analysis

step2 :: Ord a => [Flow] -> LambdaF a -> [a] -> [a]
step2 [] lambF analysis = analysis -- if W == Nil return analysis
step2 (w:ws) lambF analysis =
    let l = fstLabel w
        l' = sndLabel w
        fl = (lambF (analysis!!l)) -- get lambda function for label l 
    in if fl (analysis!!l) > (analysis!!l') then undefined  else undefined 


fstLabel :: Flow -> Int
fstLabel (Intra f) = fst f
fstLabel (Inter f) = fst f

sndLabel :: Flow -> Int
sndLabel (Inter f) = snd f
sndLabel (Intra f) = snd f