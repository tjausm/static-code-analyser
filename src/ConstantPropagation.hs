{-# LANGUAGE FlexibleInstances #-}

module ConstantPropagation where

import MFP
import AttributeGrammar
import qualified Data.Map as M
import qualified Data.Set as S

data Ztb = Int Int | Top | Bottom deriving (Eq, Show) 

instance Ord Ztb where
    compare Bottom Bottom   = EQ
    compare Bottom  _       = LT
    compare _       Bottom  = GT
    compare (Int x) (Int y) = compare x y
    compare Top     Top     = EQ
    compare Top     _       = GT
    compare _       Top     = LT 

instance {-# OVERLAPPING #-} Ord (M.Map String Ztb) where 
    compare m n = case (M.toList m) of
                  []     -> LT
                  (x:xs) -> case (M.toList n) of 
                            [] -> GT
                            (y:ys) -> if x > y then GT 
                                               else compare (M.fromList xs) (M.fromList ys) 

constantPropagationAnalysis :: [Flow] -> IF -> Int -> Int -> [String] -> M.Map Int Block -> M.Map Int String -> [(M.Map String Ztb, M.Map String Ztb)]
constantPropagationAnalysis fs interf k i vs ibmap lpmap = maximalFixedPoint (MkLattice join (bottom vs))  
                                                           (MkFlow Forward fs) interf k [i] (bottom vs) (lambdaF ibmap lpmap)

bottom :: [String] -> M.Map String Ztb
bottom vs = M.fromList (zip vs (replicate (length vs) Bottom))

join :: M.Map String Ztb -> M.Map String Ztb -> M.Map String Ztb
join = M.unionWith elementJoin

elementJoin :: Ztb -> Ztb -> Ztb
elementJoin (Int x) (Int y) | x == y = Int x
elementJoin Bottom  x                = x
elementJoin x       Bottom           = x
elementJoin _       _                = Top 

lambdaF :: M.Map Int Block -> M.Map Int String -> Int -> M.Map String Ztb -> M.Map String Ztb
lambdaF ib lp i m = transferFromBlock (M.findWithDefault (S (Skip' 0)) i ib) (M.findWithDefault "" i lp) m

-- String is the enclosing procedure name (empty if none) for the prefix of variables within procedures. 
transferFromBlock :: Block -> String -> M.Map String Ztb -> M.Map String Ztb
transferFromBlock (S (IAssign' l n v))     p m = M.insert (p ++ n) (analyseExpression v m) m 
transferFromBlock (S (Call' lc lr n ps o)) p m = m                    
transferFromBlock _                        p m = m

analyseExpression :: IExpr -> M.Map String Ztb -> Ztb
analyseExpression (IConst i)   m = Int i
analyseExpression (Var x)      m = case M.lookup x m of 
                                   Nothing     -> Top 
                                   Just Bottom -> Top 
                                   Just y      -> y
analyseExpression (Plus   l r) m = plus   (analyseExpression l m) (analyseExpression r m)
analyseExpression (Minus  l r) m = minus  (analyseExpression l m) (analyseExpression r m)
analyseExpression (Times  l r) m = times  (analyseExpression l m) (analyseExpression r m)
analyseExpression (Divide l r) m = divide (analyseExpression l m) (analyseExpression r m)

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
