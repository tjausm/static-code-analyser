{-# LANGUAGE FlexibleInstances #-}

module ConstantPropagation where

import MFP
import AttributeGrammar
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe 
import Data.List
import Debug.Trace

data Ztb = Int Int | Top | Bottom deriving (Eq, Show) 

instance Ord Ztb where
    compare Bottom Bottom   = EQ
    compare Bottom  _       = LT
    compare _       Bottom  = GT
    compare (Int x) (Int y) | x == y = EQ 
                            | otherwise = GT -- always return GT if integers differ so we apply join. 
    compare Top     Top     = EQ
    compare Top     _       = GT
    compare _       Top     = LT 

instance {-# OVERLAPPING #-} Ord (M.Map String Ztb) where 
    compare m n = case (M.toList m) of
                  []           -> LT
                  ((x1,x2):xs) -> case (M.toList n) of 
                                  []           -> GT
                                  ((y1,y2):ys) -> if x2 > y2 then GT 
                                                             else compare (M.fromList xs) (M.fromList ys) 

constantPropagationAnalysis :: [Flow] -> IF -> Int -> Int -> [String] -> M.Map Int Block -> M.Map Int String -> M.Map String ([String], String) -> [(M.Map String Ztb, M.Map String Ztb)]
constantPropagationAnalysis fs interf k i vs ibmap lpmap params = maximalFixedPoint (MkLattice join combine (bottom vs)) (MkFlow Forward fs) 
                                                                  interf k [i] (top vs) (lambdaF ibmap lpmap params)

bottom :: [String] -> M.Map String Ztb
bottom vs = M.fromList (zip vs (replicate (length vs) Bottom))

top :: [String] -> M.Map String Ztb
top vs = M.fromList (zip vs (replicate (length vs) Top))

join :: M.Map String Ztb -> M.Map String Ztb -> M.Map String Ztb
join = M.unionWith elementJoin

elementJoin :: Ztb -> Ztb -> Ztb
elementJoin (Int x) (Int y) | x == y    = Int x
                            | otherwise = Top 
elementJoin Bottom  x                = x
elementJoin x       Bottom           = x
elementJoin _       _                = Top 

combine :: M.Map String Ztb -> M.Map String Ztb -> M.Map String Ztb
combine = M.unionWith elementCombine

elementCombine :: Ztb -> Ztb -> Ztb
elementCombine a b = a

hasBottom :: [(String, Ztb)] -> Bool 
hasBottom []          = False
hasBottom ((x1, x2):xs) = (x2 == Bottom) || hasBottom xs  

lambdaF :: M.Map Int Block -> M.Map Int String -> M.Map String ([String], String) -> Int -> Int -> Bool -> M.Map String Ztb -> M.Map String Ztb
lambdaF ib lp params i end True  m = M.filterWithKey (\k a -> not (isPrefixOf (M.findWithDefault "" end lp) k)) $ trace ("CPlambda: " ++ show (transferFromBlock' (M.findWithDefault (S (Skip' 0)) i ib) (M.findWithDefault "" end lp) True m params) ++ " m: " ++ show m ) 
                                                                                                                    (transferFromBlock' (M.findWithDefault (S (Skip' 0)) i ib) (M.findWithDefault "" i lp) True m params)

lambdaF ib lp params i end False m = transferFromBlock' (M.findWithDefault (S (Skip' 0)) i ib) (M.findWithDefault "" i lp) False m params  

transferFromBlock' :: Block -> String -> Bool -> M.Map String Ztb -> M.Map String ([String], String) -> M.Map String Ztb
transferFromBlock' s p b m params = case hasBottom (M.toList m) of 
                                     True  -> m 
                                     False -> transferFromBlock s p b m params

-- String is the enclosing procedure name (empty if none) for the prefix of variables within procedures. 
transferFromBlock :: Block -> String -> Bool -> M.Map String Ztb -> M.Map String ([String], String) -> M.Map String Ztb
transferFromBlock (S (IAssign' l n v))     p b m params = if M.member n m 
                                                             then M.insert n        (analyseExpression p v m) m 
                                                             else M.insert (p ++ n) (analyseExpression p v m) m 
transferFromBlock (S (Call' lc lr n ps o)) p b m params = let (ins, out) = fromJust $ params M.!? n in
                                                            if b  
                                                                then trace "waarom komen we hier niet? " M.insert o (M.findWithDefault (Int 5) (n ++ out) m) m
                                                                else M.insert (n ++ out) Top $
                                                                foldr (\(x,y) -> M.insert (n ++ x) y) m (zip ins (map (\(I x) -> analyseExpression p x m) ps))                      
transferFromBlock _                        p b m params = m

-- String is enclosing procedure for prefixes of variables. 
analyseExpression :: String -> IExpr -> M.Map String Ztb -> Ztb
analyseExpression p (IConst i)   m = Int i
analyseExpression p (Var x)      m = case M.lookup (p ++ x) m of 
                                     Nothing     -> case M.lookup x m of 
                                                    Nothing     -> Top
                                                    Just y      -> y
                                     Just y      -> trace ("p: " ++ show p) y
analyseExpression p (Plus   l r) m = plus   (analyseExpression p l m) (analyseExpression p r m)
analyseExpression p (Minus  l r) m = minus  (analyseExpression p l m) (analyseExpression p r m)
analyseExpression p (Times  l r) m = times  (analyseExpression p l m) (analyseExpression p r m)
analyseExpression p (Divide l r) m = divide (analyseExpression p l m) (analyseExpression p r m)

plus :: Ztb -> Ztb -> Ztb
plus (Int a) (Int b) = Int (a + b)
plus Bottom  Bottom  = Bottom 
plus _       _       = Top 

minus :: Ztb -> Ztb -> Ztb
minus (Int a) (Int b) = Int (a - b)
minus Bottom  Bottom  = Bottom
minus _       _       = Top

times :: Ztb -> Ztb -> Ztb
times (Int a) (Int b) = Int (a * b)
times Bottom  Bottom  = Bottom
times _       _       = Top 

divide :: Ztb -> Ztb -> Ztb
divide (Int a) (Int 0) = Top 
divide (Int a) (Int b) = Int (a `div` b)
divide Bottom  Bottom  = Bottom
divide _       _       = Top

showBlock :: Block -> String
showBlock (S (Call' lc lr n ps o)) = "Call: " ++ show lc ++ show lr ++ show n ++ show ps ++ show o
showBlock _                        = "No call block"
