module ConstantPropagation where

import MFP
import AttributeGrammar
import qualified Data.Map as M
import qualified Data.Set as S

data Ztop = Int Int | Top deriving (Eq, Show, Ord)

constantPropagationAnalysis :: [Flow] -> Int -> [String] -> M.Map Int Block -> [(M.Map String Ztop, M.Map String Ztop)]
constantPropagationAnalysis fs i vs ibmap = maximalFixedPoint (MkLattice join (bottom vs))  
                                            (MkFlow Forward fs) [i] (bottom vs) (lambdaF ibmap)

bottom :: [String] -> M.Map String Ztop
bottom vs = M.fromList (zip vs (replicate (length vs) Top))

join :: M.Map String Ztop -> M.Map String Ztop -> M.Map String Ztop
join = M.unionWith elementJoin

elementJoin :: Ztop -> Ztop -> Ztop
elementJoin (Int x) (Int y) | x == y = Int x
elementJoin _       _                = Top 

lambdaF :: M.Map Int Block -> Int -> M.Map String Ztop -> M.Map String Ztop
lambdaF ib i m = transferFromBlock (M.findWithDefault (S (Skip' 0)) i ib) m

transferFromBlock :: Block -> M.Map String Ztop -> M.Map String Ztop
transferFromBlock (S (IAssign' l n v)) m = case (all (== Top) (M.elems m)) of
                                           True  -> m
                                           False -> M.insert n (analyseExpression v m) m                      
transferFromBlock _                    m = m

analyseExpression :: IExpr -> M.Map String Ztop -> Ztop
analyseExpression (IConst i)   m = Int i
analyseExpression (Var x)      m = case M.lookup x m of 
                                   Nothing -> Top 
                                   Just y  -> y
analyseExpression (Plus   l r) m = plus   (analyseExpression l m) (analyseExpression r m)
analyseExpression (Minus  l r) m = minus  (analyseExpression l m) (analyseExpression r m)
analyseExpression (Times  l r) m = times  (analyseExpression l m) (analyseExpression r m)
analyseExpression (Divide l r) m = divide (analyseExpression l m) (analyseExpression r m)

plus :: Ztop -> Ztop -> Ztop
plus (Int a) (Int b) = Int (a + b)
plus Top      _      = Top
plus _        Top    = Top 

minus :: Ztop -> Ztop -> Ztop
minus (Int a) (Int b) = Int (a - b)
minus Top      _      = Top
minus _        Top    = Top

times :: Ztop -> Ztop -> Ztop
times (Int a) (Int b) = Int (a * b)
times Top      _      = Top
times _        Top    = Top 

divide :: Ztop -> Ztop -> Ztop
divide (Int a) (Int 0) = Top 
divide (Int a) (Int b) = Int (a `div` b)
divide Top      _      = Top
divide _        Top    = Top
