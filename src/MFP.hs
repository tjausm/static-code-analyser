module MFP where
import AttributeGrammar ( Flow )

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
-- maximalFixedPoint :: Eq Ord a => Labels -> Bottom -> L a -> FancyF a -> F -> E -> J a -> LambdaF a -> a
-- maximalFixedPoint labels bottom l fancyF f e j lambF =
--     -- Step 1
--     let w = f
--         analysis = map (\label -> if  label `elem` e then j else bottom) labels -- set extremal labels to jota
--     -- Step 2
--     in step2 F

-- step2 :: [Flow] -> LambdaF -> [a]
-- step2 [] lambF analysis = analysis
-- step2 (w:ws) analysis = 
--     let l = fst(w)
--         l' = snd(w)
--     in if (lambF l) l == (analysis `elem` l')
--         then analysis `elem` l'  
        