module MFP where
import AttributeGrammar
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intersperse, transpose)
import Debug.Trace

type Delta = [Int]

data L a =  MkLattice (a -> a -> a) (Bottom a) -- Lattice Join
type EmbellishedL a = M.Map Delta [a]
data F = MkFlow FlowDir [Flow]
type IF = [(Int, Int, Int, Int)]
type E = [Int] -- extremal labels
type J a = a -- extremal value
type LambdaF a = Int -> Bool -> a -> a -- mapping labels to transfer functions

type Bottom a = a
data FlowDir = Backward | Forward deriving Eq-- flow direction 


-- Should a have both Eq and Ord typeclass since it should be a partial order
maximalFixedPoint :: Show a => Ord a => L a -> F -> IF -> Int -> E -> J a -> LambdaF a -> [(a,a)]
maximalFixedPoint lattice@(MkLattice _ bottom)  flow@(MkFlow _ w) interf k e j lambF  =
    -- Step 1
    let labels = genLabels w
        analysis = M.singleton [] (map (\label -> if  label `elem` e then j else bottom) labels)                       -- set extremal labels to jota, all other labels to bottom.
    -- Step 2
    in  step3 lambF flow lattice $
        step2 lattice flow interf k w [] lambF analysis

step2 :: Show a => Ord a => L a -> F -> IF -> Int -> [Flow] -> Delta -> LambdaF a -> EmbellishedL a -> EmbellishedL a
step2 _ _ _ _ [] _ _ analysis = analysis                                                                               -- if W == Nil return analysis
step2 lattice@(MkLattice join bottom) flow@(MkFlow dir f) interf k (w:ws) delta lambF analysis  =
    let l = if dir == Forward then fstLabel w else sndLabel w
        l' = if dir == Forward then sndLabel w  else fstLabel w
        fl = lambF l False                                                                                             -- get lambda function for label l 
        flclean = lambF l True                                                                                         -- Throw away local variables.  
        analysis' = replacelE delta l' ((analysis M.! delta)!!(l'-1) `join` fl ((analysis M.! delta)!!(l-1))) analysis -- update l'
        w' = if dir == Forward then filter ((l' ==) . fstLabel) f else filter ((l' ==) . sndLabel) f                   -- get all flow tuples of the form (l', _) from the flow
        calls = map getFst interf
        returns = map getThird interf

        normal = if logStep analysis (w:ws) w' l l' labelendproc calls returns delta fl $                                           -- Uncomment to trace algorithm 
                    fl ((analysis M.! delta)!!(l-1)) > (analysis M.! delta)!!(l'-1)                                    -- check if transfer function over l > l'
                    then step2 lattice flow interf k (w' ++ ws) delta lambF analysis'                                  -- recurse with updated analysis 
                    else step2 lattice flow interf k ws delta lambF analysis                                           -- recurse without updated analysis

        analysisc = replacelE delta' l' (multiJoin (map (\x -> fl ((analysis M.! delta)!!(x-1))) (getCallLabels interf l')) join bottom) initanalysis   -- fl is applied to original delta value, result is stored in the updated value. 
        analysisr = replacelE delta l' (join (flclean ((initanalysis M.! delta')!!labelendproc)) ((analysis M.! delta)!!(l-1))) analysis

        delta' = updateDelta delta l k
        labelendproc = if dir == Forward then getEndProc l interf else getEndProc l' interf

        initanalysis = if M.member delta' analysis then analysis else M.insert delta' (analysis M.! delta) analysis

    in if dir == Forward 
        then case w of 
        (Intra (a,b)) -> normal
        (Inter (a,b)) -> if l `elem` calls                                                                                                                                    -- for forward analysis, roles of calls and returns are reversed for backward. 
                            then if -- logStep initanalysis (w:ws) w' l l' labelendproc calls returns delta' fl $                                                                           -- Uncomment to trace algorithm
                                    multiJoin (map (\x -> (fl ((analysis M.! delta)!!(x-1)))) (getCallLabels interf l')) join bottom > (initanalysis M.! delta')!!(l'-1)    -- join anlysis over all entry options, transfer function is applied on original delta.
                                                                                                                                                                            -- joined value is stored in delta'.   
                                then step2 lattice flow interf k (w' ++ ws) delta' lambF analysisc                                                                          -- continue normally inside the procedure.  
                                else step2 lattice flow interf k ws         delta' lambF initanalysis                                                                       -- continue normally without updating w.
                            else step2 lattice flow interf k ws delta lambF analysis                            
        (Over  (a,b)) -> if -- logStep analysis (w:ws) w' l l' labelendproc calls returns delta fl $
                            join (flclean ((initanalysis M.! delta')!!(labelendproc))) ((analysis M.! delta)!!(l-1)) > (analysis M.! delta)!!(l'-1)
                            then step2 lattice flow interf k (w' ++ ws) delta lambF analysisr   
                            else step2 lattice flow interf k ws delta lambF analysis 
        else case w of
        (Intra (a,b)) -> normal
        (Inter (a,b)) -> if  l `elem` calls                                                                                                                                    -- for forward analysis, roles of calls and returns are reversed for backward. 
                            then if -- logStep initanalysis (w:ws) w' l l' labelendproc calls returns delta' fl $  
                                join (flclean ((initanalysis M.! delta')!!(labelendproc))) ((analysis M.! delta)!!(l-1)) > (analysis M.! delta)!!(l'-1)
                                then step2 lattice flow interf k (w' ++ ws) delta lambF analysisr   
                                else step2 lattice flow interf k ws delta lambF analysis                                                                         -- Uncomment to trace algorithm
                            else step2 lattice flow interf k ws delta lambF analysis                                  
        (Over  (a,b)) -> if  --logStep analysis (w:ws) w' l l' labelendproc calls returns delta fl $
             multiJoin (map (\x -> (fl ((analysis M.! delta)!!(x-1)))) (getCallLabels interf l')) join bottom > (initanalysis M.! delta')!!(l'-1)    -- join anlysis over all entry options, transfer function is applied on original delta.
                                                                                                                                                                            -- joined value is stored in delta'.   
                            then step2 lattice flow interf k (w' ++ ws) delta' lambF analysisc                                                                          -- continue normally inside the procedure.  
                            else step2 lattice flow interf k ws         delta' lambF initanalysis                                                                       -- continue normally without updating w.
                         
                        
-- return result as tuples of (entry,exit) value
step3' :: LambdaF a -> F -> [a] -> [(a,a)]
step3' lambF (MkFlow dir f) analysis = if dir == Forward then zip analysis analysis' else zip analysis' analysis
    where
        analysis' = zipWith (\ f a -> f a) fl analysis
        fl = [lambF i False | i <- [1 .. (length analysis)]]

--step3 :: LambdaF a -> F -> EmbellishedL a -> [[(a,a)]]
--step3 lambF flow analysis = map (step3' lambF flow) $ map snd (M.toList analysis) 

step3 :: LambdaF a -> F -> L a -> EmbellishedL a -> [(a,a)]
step3 lambF flow (MkLattice join bottom) analysis = step3' lambF flow $ map (\x -> multiJoin x join bottom) (transpose (map snd (M.toList analysis)))

-- May give error for taking tail of empty list if wrong delta values are used.
updateDelta :: Delta -> Int -> Int -> Delta
updateDelta d  l    k = l : take k d

multiJoin :: [a] -> (a -> a -> a) -> a -> a
multiJoin as join bottom = foldr join bottom as

logStep :: (Show a1, Ord a1) => M.Map [Int] [a1] -> [Flow] -> [Flow] -> Int -> Int -> Int -> [Int] -> [Int] -> Delta -> (a1 -> a1) -> a2 -> a2
logStep analysis (w:ws) w' l l' le calls returns delta fl = trace ("\n\nanalysis = " ++ M.foldrWithKey (\k a s -> "\n Key: " ++ show k ++ " Value: " ++ show a ++ s ) [] analysis)
            trace ("step2: (w:ws) = " ++ unwords (map show (w:ws)))
            trace ("step2: w' = " ++ unlines (map show w'))
            trace ("step2: l = " ++ show l  ++ ", l' = " ++ show l' ++ ", le = " ++ show le)
            trace ("step2: calls = " ++ show calls )
            trace ("step2: returns = " ++ show returns )
            trace ("step2: delta = " ++ show delta)
           -- trace ("step2: if " ++  show (fl (analysis!!(l-1))) ++ ">" ++ show (analysis!!(l'-1)) ++ " = " ++ show (fl (analysis!!(l-1)) > analysis!!(l'-1)))

-- Replaces l'th element in a list
replacel :: Int -> a -> [a] -> [a]
replacel l l' w = lhs ++ [l'] ++ rhs
    where
        lhs = take (l - 1) w
        rhs = drop l w

-- Replace the l'th element in the list corresponding to delta
replacelE :: Delta -> Int -> a -> EmbellishedL a -> EmbellishedL a
replacelE d l l' analysis = let new = replacel l l' (analysis M.! d) in M.insert d new analysis


-- Functions to easily process Flow type
instance Show Flow where
    show (Inter (a,b)) = show "( " ++ show a ++ show  ";" ++ show b ++ show ")"
    show (Intra (a,b)) = show "( " ++ show a ++ show  "," ++ show b ++ show ")"
    show (Over  (a,b)) = show "( " ++ show a ++ show  "-" ++ show b ++ show ")"
fstLabel :: Flow -> Int
fstLabel (Intra f) = fst f
fstLabel (Inter f) = fst f
fstLabel (Over  f) = fst f
sndLabel :: Flow -> Int
sndLabel (Inter f) = snd f
sndLabel (Intra f) = snd f
sndLabel (Over  f) = snd f
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
getCallLabels (i:is) l = if getSnd i == l
                            then getFst i : getCallLabels is l 
                            else getCallLabels is l 

getEndProc :: Int -> [(Int, Int, Int, Int)] -> Int
getEndProc l []     = l
getEndProc l (i:is) = if getFst i == l 
                         then getThird i
                         else getEndProc l is

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

showMFP' :: (a -> String) -> [[(a,a)]] -> String
showMFP' showPoint mfp = concat (map (showMFP showPoint) mfp)

-- intersperse 2 lists [a,b] -> [1,2] -> [a,1,b,2]
mergeList::[a]->[a]->[a]
mergeList [] [] = []
mergeList  _ [] = []
mergeList [] _  = []
mergeList (x:xs) (y:ys) = x:y:mergeList xs ys
