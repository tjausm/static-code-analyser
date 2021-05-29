

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 12 "AttributeGrammar.hs" #-}

{-# LINE 406 "AttributeGrammar.ag" #-}

data LVSet = MkSet (S.Set String)
genLambda :: M.Map Int (S.Set String) -> M.Map Int (S.Set String) -> Int -> (LVSet -> LVSet)
genLambda lvGen lvKill l = setToLVSet . (S.union $ recklessLookup l lvGen) . (flip S.difference $ recklessLookup l lvKill) . lvSetToSet
  where
    recklessLookup k m = case M.lookup k m of
      Nothing -> S.empty
      Just n -> n
    lvSetToSet (MkSet x) = x
    setToLVSet x = MkSet x
{-# LINE 25 "AttributeGrammar.hs" #-}

{-# LINE 692 "AttributeGrammar.ag" #-}

indent :: [String] -> [String]
indent = map ("  " ++)

showLabel :: Int -> String
showLabel label = "\ESC[93m" ++ reverse (go label) ++ "\ESC[0m"
  where
    go :: Int -> String
    go x
      | x < 0     = error "Negative label"
      | r == 0    = subscript !! m : ""
      | otherwise = subscript !! m : go r
      where
        (r, m) = x `divMod` 10
    subscript = "₀₁₂₃₄₅₆₇₈₉"

addSemicolon :: [String] -> [String]
addSemicolon [] = []
addSemicolon xs = init xs ++ [last xs ++ ";"]
{-# LINE 47 "AttributeGrammar.hs" #-}

{-# LINE 787 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 54 "AttributeGrammar.hs" #-}
-- BExpr -------------------------------------------------------
data BExpr = BConst (Bool)
           | BVar (String)
           | LessThan (IExpr) (IExpr)
           | GreaterThan (IExpr) (IExpr)
           | LessEqual (IExpr) (IExpr)
           | GreaterEqual (IExpr) (IExpr)
           | IEqual (IExpr) (IExpr)
           | BEqual (BExpr) (BExpr)
           | And (BExpr) (BExpr)
           | Or (BExpr) (BExpr)
           | Not (BExpr)
           deriving ( Eq,Show)
-- cata
sem_BExpr :: BExpr ->
             T_BExpr
sem_BExpr (BConst _val) =
    (sem_BExpr_BConst _val)
sem_BExpr (BVar _name) =
    (sem_BExpr_BVar _name)
sem_BExpr (LessThan _left _right) =
    (sem_BExpr_LessThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterThan _left _right) =
    (sem_BExpr_GreaterThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (LessEqual _left _right) =
    (sem_BExpr_LessEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterEqual _left _right) =
    (sem_BExpr_GreaterEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (IEqual _left _right) =
    (sem_BExpr_IEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (BEqual _left _right) =
    (sem_BExpr_BEqual (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (And _left _right) =
    (sem_BExpr_And (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Or _left _right) =
    (sem_BExpr_Or (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Not _val) =
    (sem_BExpr_Not (sem_BExpr _val))
-- semantic domain
type T_BExpr = ( (S.Set String),Int,String,BExpr)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {freeVars_Syn_BExpr :: (S.Set String),precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String,self_Syn_BExpr :: BExpr}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_BExpr _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 818 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 113 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 819 "AttributeGrammar.ag" #-}
              10
              {-# LINE 118 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 123 "AttributeGrammar.hs" #-}
              )
         _self =
             BConst val_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 821 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 140 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 822 "AttributeGrammar.ag" #-}
              10
              {-# LINE 145 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 150 "AttributeGrammar.hs" #-}
              )
         _self =
             BVar name_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 472 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 176 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 824 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 181 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 825 "AttributeGrammar.ag" #-}
              4
              {-# LINE 186 "AttributeGrammar.hs" #-}
              )
         _self =
             LessThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 474 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 216 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 827 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 221 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 828 "AttributeGrammar.ag" #-}
              4
              {-# LINE 226 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 476 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 256 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 830 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 261 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 831 "AttributeGrammar.ag" #-}
              4
              {-# LINE 266 "AttributeGrammar.hs" #-}
              )
         _self =
             LessEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 478 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 296 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 833 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 301 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 834 "AttributeGrammar.ag" #-}
              4
              {-# LINE 306 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 480 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 336 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 836 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 341 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 837 "AttributeGrammar.ag" #-}
              4
              {-# LINE 346 "AttributeGrammar.hs" #-}
              )
         _self =
             IEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 839 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 376 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 840 "AttributeGrammar.ag" #-}
              4
              {-# LINE 381 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              (S.union _leftIfreeVars _rightIfreeVars)
              {-# LINE 386 "AttributeGrammar.hs" #-}
              )
         _self =
             BEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 842 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 416 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 843 "AttributeGrammar.ag" #-}
              3
              {-# LINE 421 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              (S.union _leftIfreeVars _rightIfreeVars)
              {-# LINE 426 "AttributeGrammar.hs" #-}
              )
         _self =
             And _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: BExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 845 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 456 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 846 "AttributeGrammar.ag" #-}
              2
              {-# LINE 461 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              (S.union _leftIfreeVars _rightIfreeVars)
              {-# LINE 466 "AttributeGrammar.hs" #-}
              )
         _self =
             Or _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: BExpr
         _valIfreeVars :: (S.Set String)
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOpretty =
             ({-# LINE 848 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 491 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 849 "AttributeGrammar.ag" #-}
              10
              {-# LINE 496 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 501 "AttributeGrammar.hs" #-}
              )
         _self =
             Not _valIself
         _lhsOself =
             _self
         ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
-- Block -------------------------------------------------------
data Block = E (Expr)
           | S (Stat')
-- cata
sem_Block :: Block ->
             T_Block
sem_Block (E _expr) =
    (sem_Block_E (sem_Expr _expr))
sem_Block (S _stat) =
    (sem_Block_S (sem_Stat' _stat))
-- semantic domain
type T_Block = ( Block)
data Inh_Block = Inh_Block {}
data Syn_Block = Syn_Block {self_Syn_Block :: Block}
wrap_Block :: T_Block ->
              Inh_Block ->
              Syn_Block
wrap_Block sem (Inh_Block) =
    (let ( _lhsOself) = sem
     in  (Syn_Block _lhsOself))
sem_Block_E :: T_Expr ->
               T_Block
sem_Block_E expr_ =
    (let _lhsOself :: Block
         _exprIpretty :: String
         _exprIself :: Expr
         _self =
             E _exprIself
         _lhsOself =
             _self
         ( _exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOself))
sem_Block_S :: (T_Stat') ->
               T_Block
sem_Block_S stat_ =
    (let _lhsOself :: Block
         _statOlabelProcMapPassDown :: ( M.Map Int String )
         _statOprocMapPassDown :: ( M.Map String (Int, Int) )
         _statIfinal :: ([Int])
         _statIflow :: ([Flow])
         _statIinit :: Int
         _statIinterflow :: ([(Int, Int, Int, Int)])
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIlabelBlockMapCollect :: ( M.Map Int Block )
         _statIlabels :: ( [Int] )
         _statIlvGen :: (M.Map Int (S.Set String))
         _statIlvKill :: (M.Map Int (S.Set String))
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _statIvars :: ([String])
         _self =
             S _statIself
         _lhsOself =
             _self
         _statOlabelProcMapPassDown =
             ({-# LINE 541 "AttributeGrammar.ag" #-}
              error "missing rule: Block.S.stat.labelProcMapPassDown"
              {-# LINE 569 "AttributeGrammar.hs" #-}
              )
         _statOprocMapPassDown =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
              error "missing rule: Block.S.stat.procMapPassDown"
              {-# LINE 574 "AttributeGrammar.hs" #-}
              )
         ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlabels,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
             stat_ _statOlabelProcMapPassDown _statOprocMapPassDown
     in  ( _lhsOself))
-- Code --------------------------------------------------------
data Code = CBExpr (BExpr)
          | CIExpr (IExpr)
          | CStat (Stat')
          | CProc (Proc')
          | CProgram (Program')
-- cata
sem_Code :: Code ->
            T_Code
sem_Code (CBExpr _bExpr) =
    (sem_Code_CBExpr (sem_BExpr _bExpr))
sem_Code (CIExpr _iExpr) =
    (sem_Code_CIExpr (sem_IExpr _iExpr))
sem_Code (CStat _stat') =
    (sem_Code_CStat (sem_Stat' _stat'))
sem_Code (CProc _proc') =
    (sem_Code_CProc (sem_Proc' _proc'))
sem_Code (CProgram _program') =
    (sem_Code_CProgram (sem_Program' _program'))
-- semantic domain
type T_Code = ( Code)
data Inh_Code = Inh_Code {}
data Syn_Code = Syn_Code {self_Syn_Code :: Code}
wrap_Code :: T_Code ->
             Inh_Code ->
             Syn_Code
wrap_Code sem (Inh_Code) =
    (let ( _lhsOself) = sem
     in  (Syn_Code _lhsOself))
sem_Code_CBExpr :: T_BExpr ->
                   T_Code
sem_Code_CBExpr bExpr_ =
    (let _lhsOself :: Code
         _bExprIfreeVars :: (S.Set String)
         _bExprIprecedence :: Int
         _bExprIpretty :: String
         _bExprIself :: BExpr
         _self =
             CBExpr _bExprIself
         _lhsOself =
             _self
         ( _bExprIfreeVars,_bExprIprecedence,_bExprIpretty,_bExprIself) =
             bExpr_
     in  ( _lhsOself))
sem_Code_CIExpr :: T_IExpr ->
                   T_Code
sem_Code_CIExpr iExpr_ =
    (let _lhsOself :: Code
         _iExprIfreeVars :: (S.Set String)
         _iExprIprecedence :: Int
         _iExprIpretty :: String
         _iExprIself :: IExpr
         _self =
             CIExpr _iExprIself
         _lhsOself =
             _self
         ( _iExprIfreeVars,_iExprIprecedence,_iExprIpretty,_iExprIself) =
             iExpr_
     in  ( _lhsOself))
sem_Code_CStat :: (T_Stat') ->
                  T_Code
sem_Code_CStat stat'_ =
    (let _lhsOself :: Code
         _stat'OlabelProcMapPassDown :: ( M.Map Int String )
         _stat'OprocMapPassDown :: ( M.Map String (Int, Int) )
         _stat'Ifinal :: ([Int])
         _stat'Iflow :: ([Flow])
         _stat'Iinit :: Int
         _stat'Iinterflow :: ([(Int, Int, Int, Int)])
         _stat'IisSingle :: Bool
         _stat'IisSkip :: Bool
         _stat'IlabelBlockMapCollect :: ( M.Map Int Block )
         _stat'Ilabels :: ( [Int] )
         _stat'IlvGen :: (M.Map Int (S.Set String))
         _stat'IlvKill :: (M.Map Int (S.Set String))
         _stat'Ipretty :: ( [String] )
         _stat'Iself :: Stat'
         _stat'Ivars :: ([String])
         _self =
             CStat _stat'Iself
         _lhsOself =
             _self
         _stat'OlabelProcMapPassDown =
             ({-# LINE 541 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CStat.stat'.labelProcMapPassDown"
              {-# LINE 664 "AttributeGrammar.hs" #-}
              )
         _stat'OprocMapPassDown =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CStat.stat'.procMapPassDown"
              {-# LINE 669 "AttributeGrammar.hs" #-}
              )
         ( _stat'Ifinal,_stat'Iflow,_stat'Iinit,_stat'Iinterflow,_stat'IisSingle,_stat'IisSkip,_stat'IlabelBlockMapCollect,_stat'Ilabels,_stat'IlvGen,_stat'IlvKill,_stat'Ipretty,_stat'Iself,_stat'Ivars) =
             stat'_ _stat'OlabelProcMapPassDown _stat'OprocMapPassDown
     in  ( _lhsOself))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let _lhsOself :: Code
         _proc'OlabelProcMapPassDown :: ( M.Map Int String )
         _proc'OprocMapPassDown :: ( M.Map String (Int, Int) )
         _proc'Ifinal :: ([Int])
         _proc'Iflow :: ([Flow])
         _proc'Iinit :: Int
         _proc'Iinterflow :: ([(Int, Int, Int, Int)])
         _proc'IlabelBlockMapCollect :: ( M.Map Int Block )
         _proc'IlabelProcMapCollect :: ( M.Map Int String )
         _proc'Ipretty :: ( [String] )
         _proc'IprocMapCollect :: ( M.Map String (Int, Int) )
         _proc'Iself :: Proc'
         _proc'Ivars :: ([String])
         _self =
             CProc _proc'Iself
         _lhsOself =
             _self
         _proc'OlabelProcMapPassDown =
             ({-# LINE 541 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CProc.proc'.labelProcMapPassDown"
              {-# LINE 697 "AttributeGrammar.hs" #-}
              )
         _proc'OprocMapPassDown =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CProc.proc'.procMapPassDown"
              {-# LINE 702 "AttributeGrammar.hs" #-}
              )
         ( _proc'Ifinal,_proc'Iflow,_proc'Iinit,_proc'Iinterflow,_proc'IlabelBlockMapCollect,_proc'IlabelProcMapCollect,_proc'Ipretty,_proc'IprocMapCollect,_proc'Iself,_proc'Ivars) =
             proc'_ _proc'OlabelProcMapPassDown _proc'OprocMapPassDown
     in  ( _lhsOself))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let _lhsOself :: Code
         _program'Ifinal :: ([Int])
         _program'Iflow :: ([Flow])
         _program'Iinit :: Int
         _program'Iinterflow :: ([(Int, Int, Int, Int)])
         _program'IlabelBlockMapCollect :: ( M.Map Int Block )
         _program'IlabelProcMapCollect :: ( M.Map Int String )
         _program'IlvGen :: (M.Map Int (S.Set String))
         _program'IlvKill :: (M.Map Int (S.Set String))
         _program'IlvLambda :: ( Int -> (LVSet-> LVSet) )
         _program'Ipretty :: String
         _program'IprocMapCollect :: ( M.Map String (Int, Int) )
         _program'Iself :: Program'
         _program'Ivars :: ([String])
         _self =
             CProgram _program'Iself
         _lhsOself =
             _self
         ( _program'Ifinal,_program'Iflow,_program'Iinit,_program'Iinterflow,_program'IlabelBlockMapCollect,_program'IlabelProcMapCollect,_program'IlvGen,_program'IlvKill,_program'IlvLambda,_program'Ipretty,_program'IprocMapCollect,_program'Iself,_program'Ivars) =
             program'_
     in  ( _lhsOself))
-- Expr --------------------------------------------------------
data Expr = B (BExpr)
          | I (IExpr)
          deriving ( Eq,Show)
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (B _expr) =
    (sem_Expr_B (sem_BExpr _expr))
sem_Expr (I _expr) =
    (sem_Expr_I (sem_IExpr _expr))
-- semantic domain
type T_Expr = ( String,Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {pretty_Syn_Expr :: String,self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Expr _lhsOpretty _lhsOself))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B expr_ =
    (let _lhsOpretty :: String
         _lhsOself :: Expr
         _exprIfreeVars :: (S.Set String)
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: BExpr
         _lhsOpretty =
             ({-# LINE 853 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 764 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         ( _exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOpretty,_lhsOself))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOpretty :: String
         _lhsOself :: Expr
         _exprIfreeVars :: (S.Set String)
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: IExpr
         _lhsOpretty =
             ({-# LINE 855 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 785 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         ( _exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOpretty,_lhsOself))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( String,Exprs)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {pretty_Syn_Exprs :: String,self_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Exprs _lhsOpretty _lhsOself))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOpretty :: String
         _lhsOself :: Exprs
         _hdIpretty :: String
         _hdIself :: Expr
         _tlIpretty :: String
         _tlIself :: Exprs
         _lhsOpretty =
             ({-# LINE 861 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 824 "AttributeGrammar.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpretty,_hdIself) =
             hd_
         ( _tlIpretty,_tlIself) =
             tl_
     in  ( _lhsOpretty,_lhsOself))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOpretty :: String
         _lhsOself :: Exprs
         _lhsOpretty =
             ({-# LINE 859 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 842 "AttributeGrammar.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
-- Flow --------------------------------------------------------
data Flow = Intra (((Int, Int)))
          | Inter (((Int, Int)))
-- cata
sem_Flow :: Flow ->
            T_Flow
sem_Flow (Intra _labels) =
    (sem_Flow_Intra _labels)
sem_Flow (Inter _labels) =
    (sem_Flow_Inter _labels)
-- semantic domain
type T_Flow = ( Flow)
data Inh_Flow = Inh_Flow {}
data Syn_Flow = Syn_Flow {self_Syn_Flow :: Flow}
wrap_Flow :: T_Flow ->
             Inh_Flow ->
             Syn_Flow
wrap_Flow sem (Inh_Flow) =
    (let ( _lhsOself) = sem
     in  (Syn_Flow _lhsOself))
sem_Flow_Intra :: ((Int, Int)) ->
                  T_Flow
sem_Flow_Intra labels_ =
    (let _lhsOself :: Flow
         _self =
             Intra labels_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Flow_Inter :: ((Int, Int)) ->
                  T_Flow
sem_Flow_Inter labels_ =
    (let _lhsOself :: Flow
         _self =
             Inter labels_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- IExpr -------------------------------------------------------
data IExpr = IConst (Int)
           | Var (String)
           | Plus (IExpr) (IExpr)
           | Minus (IExpr) (IExpr)
           | Times (IExpr) (IExpr)
           | Divide (IExpr) (IExpr)
           | Deref (IExpr)
           deriving ( Eq,Show)
-- cata
sem_IExpr :: IExpr ->
             T_IExpr
sem_IExpr (IConst _val) =
    (sem_IExpr_IConst _val)
sem_IExpr (Var _name) =
    (sem_IExpr_Var _name)
sem_IExpr (Plus _left _right) =
    (sem_IExpr_Plus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Minus _left _right) =
    (sem_IExpr_Minus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Times _left _right) =
    (sem_IExpr_Times (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Divide _left _right) =
    (sem_IExpr_Divide (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Deref _ptr) =
    (sem_IExpr_Deref (sem_IExpr _ptr))
-- semantic domain
type T_IExpr = ( (S.Set String),Int,String,IExpr)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {freeVars_Syn_IExpr :: (S.Set String),precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String,self_Syn_IExpr :: IExpr}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_IExpr _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 795 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 933 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 796 "AttributeGrammar.ag" #-}
              10
              {-# LINE 938 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 943 "AttributeGrammar.hs" #-}
              )
         _self =
             IConst val_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 460 "AttributeGrammar.ag" #-}
              S.singleton name_
              {-# LINE 960 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 798 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 965 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 799 "AttributeGrammar.ag" #-}
              10
              {-# LINE 970 "AttributeGrammar.hs" #-}
              )
         _self =
             Var name_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 462 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 996 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 801 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 1001 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 802 "AttributeGrammar.ag" #-}
              6
              {-# LINE 1006 "AttributeGrammar.hs" #-}
              )
         _self =
             Plus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 464 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 1036 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 804 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 1041 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 805 "AttributeGrammar.ag" #-}
              6
              {-# LINE 1046 "AttributeGrammar.hs" #-}
              )
         _self =
             Minus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 466 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 1076 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 807 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1081 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 808 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1086 "AttributeGrammar.hs" #-}
              )
         _self =
             Times _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOfreeVars =
             ({-# LINE 468 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 1116 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 810 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1121 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 811 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1126 "AttributeGrammar.hs" #-}
              )
         _self =
             Divide _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOself :: IExpr
         _ptrIfreeVars :: (S.Set String)
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 813 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1151 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 814 "AttributeGrammar.ag" #-}
              10
              {-# LINE 1156 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 1161 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
             ptr_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
-- Proc --------------------------------------------------------
data Proc = Proc (String) (([String])) (String) (Stat)
          deriving ( Show)
-- cata
sem_Proc :: Proc ->
            T_Proc
sem_Proc (Proc _name _inp _out _stat) =
    (sem_Proc_Proc _name _inp _out (sem_Stat _stat))
-- semantic domain
type T_Proc = Int ->
              ( Int,Proc',Proc)
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Int}
data Syn_Proc = Syn_Proc {label_Syn_Proc :: Int,labelled_Syn_Proc :: Proc',self_Syn_Proc :: Proc}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Proc _lhsOlabel _lhsOlabelled _lhsOself))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel ->
         (let _statOlabel :: Int
              _lhsOlabelled :: Proc'
              _lhsOlabel :: Int
              _lhsOself :: Proc
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statIself :: Stat
              _statOlabel =
                  ({-# LINE 628 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1206 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 629 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 1211 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 630 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 1216 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIlabel,_statIlabelled,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Proc' -------------------------------------------------------
data Proc' = Proc' (Int) (Int) (String) (([String])) (String) (Stat')
           deriving ( Show)
-- cata
sem_Proc' :: (Proc') ->
             (T_Proc')
sem_Proc' (Proc' _labelEntry _labelReturn _name _inp _out _stat) =
    (sem_Proc'_Proc' _labelEntry _labelReturn _name _inp _out (sem_Stat' _stat))
-- semantic domain
type T_Proc' = ( M.Map Int String ) ->
               ( M.Map String (Int, Int) ) ->
               ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),( M.Map Int Block ),( M.Map Int String ),( [String] ),( M.Map String (Int, Int) ),Proc',([String]))
data Inh_Proc' = Inh_Proc' {labelProcMapPassDown_Inh_Proc' :: ( M.Map Int String ),procMapPassDown_Inh_Proc' :: ( M.Map String (Int, Int) )}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ([Int]),flow_Syn_Proc' :: ([Flow]),init_Syn_Proc' :: Int,interflow_Syn_Proc' :: ([(Int, Int, Int, Int)]),labelBlockMapCollect_Syn_Proc' :: ( M.Map Int Block ),labelProcMapCollect_Syn_Proc' :: ( M.Map Int String ),pretty_Syn_Proc' :: ( [String] ),procMapCollect_Syn_Proc' :: ( M.Map String (Int, Int) ),self_Syn_Proc' :: Proc',vars_Syn_Proc' :: ([String])}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIlabelProcMapPassDown _lhsIprocMapPassDown) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlabelProcMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars) = sem _lhsIlabelProcMapPassDown _lhsIprocMapPassDown
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOlabelBlockMapCollect _lhsOlabelProcMapCollect _lhsOpretty _lhsOprocMapCollect _lhsOself _lhsOvars))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelReturn_ name_ inp_ out_ stat_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
              _statOprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabelProcMapCollect :: ( M.Map Int String )
              _statOlabelProcMapPassDown :: ( M.Map Int String )
              _lhsOpretty :: ( [String] )
              _lhsOself :: Proc'
              _statIfinal :: ([Int])
              _statIflow :: ([Flow])
              _statIinit :: Int
              _statIinterflow :: ([(Int, Int, Int, Int)])
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIlabelBlockMapCollect :: ( M.Map Int Block )
              _statIlabels :: ( [Int] )
              _statIlvGen :: (M.Map Int (S.Set String))
              _statIlvKill :: (M.Map Int (S.Set String))
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIvars :: ([String])
              _lhsOinit =
                  ({-# LINE 113 "AttributeGrammar.ag" #-}
                   labelEntry_
                   {-# LINE 1283 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   [labelReturn_]
                   {-# LINE 1288 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 209 "AttributeGrammar.ag" #-}
                   Intra (labelEntry_, _statIinit) : _statIflow ++ map (\x -> Intra (x, labelReturn_)) _statIfinal
                   {-# LINE 1293 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 265 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1298 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   M.singleton name_ (labelEntry_, labelReturn_)
                   {-# LINE 1303 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1308 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 358 "AttributeGrammar.ag" #-}
                   L.nub (map (\x -> name_ ++ x) _statIvars)
                   {-# LINE 1313 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 492 "AttributeGrammar.ag" #-}
                   _statIlabelBlockMapCollect
                   {-# LINE 1318 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelProcMapCollect =
                  ({-# LINE 551 "AttributeGrammar.ag" #-}
                   M.union (M.union (M.singleton labelEntry_ name_) (M.singleton labelReturn_ name_))
                   (M.fromList (zip _statIlabels (replicate (length _statIlabels) name_)))
                   {-# LINE 1324 "AttributeGrammar.hs" #-}
                   )
              _statOlabelProcMapPassDown =
                  ({-# LINE 553 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 1329 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 725 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelReturn_ ++ ";"]
                   {-# LINE 1336 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelReturn_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlabels,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
                  stat_ _statOlabelProcMapPassDown _statOprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlabelProcMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars)))
-- Procs -------------------------------------------------------
type Procs = [Proc]
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs list =
    (Prelude.foldr sem_Procs_Cons sem_Procs_Nil (Prelude.map sem_Proc list))
-- semantic domain
type T_Procs = Int ->
               ( Int,Procs',Procs)
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Int}
data Syn_Procs = Syn_Procs {label_Syn_Procs :: Int,labelled_Syn_Procs :: Procs',self_Syn_Procs :: Procs}
wrap_Procs :: T_Procs ->
              Inh_Procs ->
              Syn_Procs
wrap_Procs sem (Inh_Procs _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Procs _lhsOlabel _lhsOlabelled _lhsOself))
sem_Procs_Cons :: T_Proc ->
                  T_Procs ->
                  T_Procs
sem_Procs_Cons hd_ tl_ =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _hdOlabel :: Int
              _tlOlabel :: Int
              _hdIlabel :: Int
              _hdIlabelled :: Proc'
              _hdIself :: Proc
              _tlIlabel :: Int
              _tlIlabelled :: Procs'
              _tlIself :: Procs
              _lhsOlabelled =
                  ({-# LINE 624 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1382 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 607 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1391 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 607 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1396 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 607 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1401 "AttributeGrammar.hs" #-}
                   )
              ( _hdIlabel,_hdIlabelled,_hdIself) =
                  hd_ _hdOlabel
              ( _tlIlabel,_tlIlabelled,_tlIself) =
                  tl_ _tlOlabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _lhsOlabelled =
                  ({-# LINE 622 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1417 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 607 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1426 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Procs' ------------------------------------------------------
type Procs' = [Proc']
-- cata
sem_Procs' :: (Procs') ->
              (T_Procs')
sem_Procs' list =
    (Prelude.foldr sem_Procs'_Cons sem_Procs'_Nil (Prelude.map sem_Proc' list))
-- semantic domain
type T_Procs' = ( M.Map Int String ) ->
                ( M.Map String (Int, Int) ) ->
                ( ([Flow]),([(Int, Int, Int, Int)]),( M.Map Int Block ),( M.Map Int String ),( [String] ),( M.Map String (Int, Int) ),Procs',([String]))
data Inh_Procs' = Inh_Procs' {labelProcMapPassDown_Inh_Procs' :: ( M.Map Int String ),procMapPassDown_Inh_Procs' :: ( M.Map String (Int, Int) )}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ([Flow]),interflow_Syn_Procs' :: ([(Int, Int, Int, Int)]),labelBlockMapCollect_Syn_Procs' :: ( M.Map Int Block ),labelProcMapCollect_Syn_Procs' :: ( M.Map Int String ),pretty_Syn_Procs' :: ( [String] ),procMapCollect_Syn_Procs' :: ( M.Map String (Int, Int) ),self_Syn_Procs' :: Procs',vars_Syn_Procs' :: ([String])}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIlabelProcMapPassDown _lhsIprocMapPassDown) =
    (let ( _lhsOflow,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlabelProcMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars) = sem _lhsIlabelProcMapPassDown _lhsIprocMapPassDown
     in  (Syn_Procs' _lhsOflow _lhsOinterflow _lhsOlabelBlockMapCollect _lhsOlabelProcMapCollect _lhsOpretty _lhsOprocMapCollect _lhsOself _lhsOvars))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
              _hdOprocMapPassDown :: ( M.Map String (Int, Int) )
              _tlOprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabelProcMapCollect :: ( M.Map Int String )
              _hdOlabelProcMapPassDown :: ( M.Map Int String )
              _tlOlabelProcMapPassDown :: ( M.Map Int String )
              _lhsOpretty :: ( [String] )
              _lhsOself :: Procs'
              _hdIfinal :: ([Int])
              _hdIflow :: ([Flow])
              _hdIinit :: Int
              _hdIinterflow :: ([(Int, Int, Int, Int)])
              _hdIlabelBlockMapCollect :: ( M.Map Int Block )
              _hdIlabelProcMapCollect :: ( M.Map Int String )
              _hdIpretty :: ( [String] )
              _hdIprocMapCollect :: ( M.Map String (Int, Int) )
              _hdIself :: Proc'
              _hdIvars :: ([String])
              _tlIflow :: ([Flow])
              _tlIinterflow :: ([(Int, Int, Int, Int)])
              _tlIlabelBlockMapCollect :: ( M.Map Int Block )
              _tlIlabelProcMapCollect :: ( M.Map Int String )
              _tlIpretty :: ( [String] )
              _tlIprocMapCollect :: ( M.Map String (Int, Int) )
              _tlIself :: Procs'
              _tlIvars :: ([String])
              _lhsOflow =
                  ({-# LINE 215 "AttributeGrammar.ag" #-}
                   _hdIflow ++ _tlIflow
                   {-# LINE 1487 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 271 "AttributeGrammar.ag" #-}
                   _hdIinterflow ++ _tlIinterflow
                   {-# LINE 1492 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   M.union _hdIprocMapCollect _tlIprocMapCollect
                   {-# LINE 1497 "AttributeGrammar.hs" #-}
                   )
              _hdOprocMapPassDown =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1502 "AttributeGrammar.hs" #-}
                   )
              _tlOprocMapPassDown =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1507 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   L.nub (_hdIvars ++ _tlIvars)
                   {-# LINE 1512 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 498 "AttributeGrammar.ag" #-}
                   M.union _hdIlabelBlockMapCollect _tlIlabelBlockMapCollect
                   {-# LINE 1517 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelProcMapCollect =
                  ({-# LINE 559 "AttributeGrammar.ag" #-}
                   M.union _hdIlabelProcMapCollect _tlIlabelProcMapCollect
                   {-# LINE 1522 "AttributeGrammar.hs" #-}
                   )
              _hdOlabelProcMapPassDown =
                  ({-# LINE 560 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 1527 "AttributeGrammar.hs" #-}
                   )
              _tlOlabelProcMapPassDown =
                  ({-# LINE 561 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 1532 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 721 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1537 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIinterflow,_hdIlabelBlockMapCollect,_hdIlabelProcMapCollect,_hdIpretty,_hdIprocMapCollect,_hdIself,_hdIvars) =
                  hd_ _hdOlabelProcMapPassDown _hdOprocMapPassDown
              ( _tlIflow,_tlIinterflow,_tlIlabelBlockMapCollect,_tlIlabelProcMapCollect,_tlIpretty,_tlIprocMapCollect,_tlIself,_tlIvars) =
                  tl_ _tlOlabelProcMapPassDown _tlOprocMapPassDown
          in  ( _lhsOflow,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlabelProcMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabelProcMapCollect :: ( M.Map Int String )
              _lhsOpretty :: ( [String] )
              _lhsOself :: Procs'
              _lhsOflow =
                  ({-# LINE 213 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1563 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1568 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1573 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1578 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 496 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1583 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelProcMapCollect =
                  ({-# LINE 557 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1588 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 719 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1593 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOflow,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlabelProcMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars)))
-- Program -----------------------------------------------------
data Program = Program (Procs) (Stat)
             deriving ( Show)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Program _procs _stat) =
    (sem_Program_Program (sem_Procs _procs) (sem_Stat _stat))
-- semantic domain
type T_Program = ( Program',Program)
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {labelled_Syn_Program :: Program',self_Syn_Program :: Program}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program) =
    (let ( _lhsOlabelled,_lhsOself) = sem
     in  (Syn_Program _lhsOlabelled _lhsOself))
sem_Program_Program :: T_Procs ->
                       T_Stat ->
                       T_Program
sem_Program_Program procs_ stat_ =
    (let _procsOlabel :: Int
         _statOlabel :: Int
         _lhsOlabelled :: Program'
         _lhsOself :: Program
         _procsIlabel :: Int
         _procsIlabelled :: Procs'
         _procsIself :: Procs
         _statIlabel :: Int
         _statIlabelled :: Stat'
         _statIself :: Stat
         _procsOlabel =
             ({-# LINE 616 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1635 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 617 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1640 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 618 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 1645 "AttributeGrammar.hs" #-}
              )
         _self =
             Program _procsIself _statIself
         _lhsOself =
             _self
         ( _procsIlabel,_procsIlabelled,_procsIself) =
             procs_ _procsOlabel
         ( _statIlabel,_statIlabelled,_statIself) =
             stat_ _statOlabel
     in  ( _lhsOlabelled,_lhsOself))
-- Program' ----------------------------------------------------
data Program' = Program' (Procs') (Stat')
              deriving ( Show)
-- cata
sem_Program' :: (Program') ->
                (T_Program')
sem_Program' (Program' _procs _stat) =
    (sem_Program'_Program' (sem_Procs' _procs) (sem_Stat' _stat))
-- semantic domain
type T_Program' = ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),( M.Map Int Block ),( M.Map Int String ),(M.Map Int (S.Set String)),(M.Map Int (S.Set String)),( Int -> (LVSet-> LVSet) ),String,( M.Map String (Int, Int) ),Program',([String]))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ([Int]),flow_Syn_Program' :: ([Flow]),init_Syn_Program' :: Int,interflow_Syn_Program' :: ([(Int, Int, Int, Int)]),labelBlockMapCollect_Syn_Program' :: ( M.Map Int Block ),labelProcMapCollect_Syn_Program' :: ( M.Map Int String ),lvGen_Syn_Program' :: (M.Map Int (S.Set String)),lvKill_Syn_Program' :: (M.Map Int (S.Set String)),lvLambda_Syn_Program' :: ( Int -> (LVSet-> LVSet) ),pretty_Syn_Program' :: String,procMapCollect_Syn_Program' :: ( M.Map String (Int, Int) ),self_Syn_Program' :: Program',vars_Syn_Program' :: ([String])}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlabelProcMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOlvLambda,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOlabelBlockMapCollect _lhsOlabelProcMapCollect _lhsOlvGen _lhsOlvKill _lhsOlvLambda _lhsOpretty _lhsOprocMapCollect _lhsOself _lhsOvars))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOflow :: ([Flow])
         _lhsOinterflow :: ([(Int, Int, Int, Int)])
         _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
         _statOprocMapPassDown :: ( M.Map String (Int, Int) )
         _procsOprocMapPassDown :: ( M.Map String (Int, Int) )
         _lhsOvars :: ([String])
         _lhsOlvLambda :: ( Int -> (LVSet-> LVSet) )
         _lhsOlvKill :: (M.Map Int (S.Set String))
         _lhsOlvGen :: (M.Map Int (S.Set String))
         _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
         _lhsOlabelProcMapCollect :: ( M.Map Int String )
         _statOlabelProcMapPassDown :: ( M.Map Int String )
         _procsOlabelProcMapPassDown :: ( M.Map Int String )
         _lhsOpretty :: String
         _lhsOself :: Program'
         _procsIflow :: ([Flow])
         _procsIinterflow :: ([(Int, Int, Int, Int)])
         _procsIlabelBlockMapCollect :: ( M.Map Int Block )
         _procsIlabelProcMapCollect :: ( M.Map Int String )
         _procsIpretty :: ( [String] )
         _procsIprocMapCollect :: ( M.Map String (Int, Int) )
         _procsIself :: Procs'
         _procsIvars :: ([String])
         _statIfinal :: ([Int])
         _statIflow :: ([Flow])
         _statIinit :: Int
         _statIinterflow :: ([(Int, Int, Int, Int)])
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIlabelBlockMapCollect :: ( M.Map Int Block )
         _statIlabels :: ( [Int] )
         _statIlvGen :: (M.Map Int (S.Set String))
         _statIlvKill :: (M.Map Int (S.Set String))
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _statIvars :: ([String])
         _lhsOinit =
             ({-# LINE 109 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1719 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 157 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1724 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 205 "AttributeGrammar.ag" #-}
              _statIflow ++ _procsIflow
              {-# LINE 1729 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 261 "AttributeGrammar.ag" #-}
              _statIinterflow ++ _procsIinterflow
              {-# LINE 1734 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 318 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1739 "AttributeGrammar.hs" #-}
              )
         _statOprocMapPassDown =
             ({-# LINE 319 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1744 "AttributeGrammar.hs" #-}
              )
         _procsOprocMapPassDown =
             ({-# LINE 320 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1749 "AttributeGrammar.hs" #-}
              )
         _lhsOvars =
             ({-# LINE 353 "AttributeGrammar.ag" #-}
              L.nub (_statIvars ++ _procsIvars)
              {-# LINE 1754 "AttributeGrammar.hs" #-}
              )
         _lhsOlvLambda =
             ({-# LINE 425 "AttributeGrammar.ag" #-}
              genLambda _statIlvGen _statIlvKill
              {-# LINE 1759 "AttributeGrammar.hs" #-}
              )
         _lhsOlvKill =
             ({-# LINE 430 "AttributeGrammar.ag" #-}
              _statIlvKill
              {-# LINE 1764 "AttributeGrammar.hs" #-}
              )
         _lhsOlvGen =
             ({-# LINE 446 "AttributeGrammar.ag" #-}
              _statIlvGen
              {-# LINE 1769 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelBlockMapCollect =
             ({-# LINE 488 "AttributeGrammar.ag" #-}
              M.union _procsIlabelBlockMapCollect _statIlabelBlockMapCollect
              {-# LINE 1774 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelProcMapCollect =
             ({-# LINE 545 "AttributeGrammar.ag" #-}
              _procsIlabelProcMapCollect
              {-# LINE 1779 "AttributeGrammar.hs" #-}
              )
         _statOlabelProcMapPassDown =
             ({-# LINE 546 "AttributeGrammar.ag" #-}
              _procsIlabelProcMapCollect
              {-# LINE 1784 "AttributeGrammar.hs" #-}
              )
         _procsOlabelProcMapPassDown =
             ({-# LINE 547 "AttributeGrammar.ag" #-}
              _procsIlabelProcMapCollect
              {-# LINE 1789 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 715 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1794 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself
         _lhsOself =
             _self
         ( _procsIflow,_procsIinterflow,_procsIlabelBlockMapCollect,_procsIlabelProcMapCollect,_procsIpretty,_procsIprocMapCollect,_procsIself,_procsIvars) =
             procs_ _procsOlabelProcMapPassDown _procsOprocMapPassDown
         ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlabels,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
             stat_ _statOlabelProcMapPassDown _statOprocMapPassDown
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlabelProcMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOlvLambda,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars))
-- Stat --------------------------------------------------------
data Stat = Skip
          | IfThenElse (BExpr) (Stat) (Stat)
          | While (BExpr) (Stat)
          | Call (String) (Exprs) (String)
          | IAssign (String) (IExpr)
          | BAssign (String) (BExpr)
          | Seq (Stat) (Stat)
          | Malloc (String) (IExpr)
          | Free (IExpr)
          | RefAssign (IExpr) (IExpr)
          | Continue
          | Break
          deriving ( Show)
-- cata
sem_Stat :: Stat ->
            T_Stat
sem_Stat (Skip) =
    (sem_Stat_Skip)
sem_Stat (IfThenElse _cond _stat1 _stat2) =
    (sem_Stat_IfThenElse _cond (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (While _cond _stat) =
    (sem_Stat_While _cond (sem_Stat _stat))
sem_Stat (Call _name _params _out) =
    (sem_Stat_Call _name _params _out)
sem_Stat (IAssign _name _val) =
    (sem_Stat_IAssign _name _val)
sem_Stat (BAssign _name _val) =
    (sem_Stat_BAssign _name _val)
sem_Stat (Seq _stat1 _stat2) =
    (sem_Stat_Seq (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (Malloc _name _size) =
    (sem_Stat_Malloc _name _size)
sem_Stat (Free _ptr) =
    (sem_Stat_Free _ptr)
sem_Stat (RefAssign _ptr _val) =
    (sem_Stat_RefAssign _ptr _val)
sem_Stat (Continue) =
    (sem_Stat_Continue)
sem_Stat (Break) =
    (sem_Stat_Break)
-- semantic domain
type T_Stat = Int ->
              ( Int,Stat',Stat)
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Int}
data Syn_Stat = Syn_Stat {label_Syn_Stat :: Int,labelled_Syn_Stat :: Stat',self_Syn_Stat :: Stat}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Stat _lhsOlabel _lhsOlabelled _lhsOself))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 634 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1866 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 635 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1871 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_IfThenElse :: BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _stat1Olabel :: Int
              _stat2Olabel :: Int
              _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _stat1Ilabel :: Int
              _stat1Ilabelled :: Stat'
              _stat1Iself :: Stat
              _stat2Ilabel :: Int
              _stat2Ilabelled :: Stat'
              _stat2Iself :: Stat
              _stat1Olabel =
                  ({-# LINE 638 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1898 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 639 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1903 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 640 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1908 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 641 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1913 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse cond_ _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Ilabel,_stat1Ilabelled,_stat1Iself) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Ilabelled,_stat2Iself) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_While :: BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel ->
         (let _statOlabel :: Int
              _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statIself :: Stat
              _statOlabel =
                  ({-# LINE 644 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1939 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 645 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1944 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 646 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1949 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While cond_ _statIself
              _lhsOself =
                  _self
              ( _statIlabel,_statIlabelled,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Call :: String ->
                 Exprs ->
                 String ->
                 T_Stat
sem_Stat_Call name_ params_ out_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 649 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1970 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 650 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1975 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call name_ params_ out_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_IAssign :: String ->
                    IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 653 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1993 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 654 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1998 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign name_ val_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_BAssign :: String ->
                    BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 657 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2016 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 658 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 2021 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign name_ val_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _stat1Olabel :: Int
              _stat2Olabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel :: Int
              _stat1Ilabel :: Int
              _stat1Ilabelled :: Stat'
              _stat1Iself :: Stat
              _stat2Ilabel :: Int
              _stat2Ilabelled :: Stat'
              _stat2Iself :: Stat
              _stat1Olabel =
                  ({-# LINE 661 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 2047 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 662 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 2052 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 663 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 2057 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 607 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 2066 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ilabel,_stat1Ilabelled,_stat1Iself) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Ilabelled,_stat2Iself) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Malloc :: String ->
                   IExpr ->
                   T_Stat
sem_Stat_Malloc name_ size_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 666 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2084 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 667 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 2089 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc name_ size_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 670 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2106 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 671 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 2111 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free ptr_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_RefAssign :: IExpr ->
                      IExpr ->
                      T_Stat
sem_Stat_RefAssign ptr_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 674 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2129 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 675 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 2134 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign ptr_ val_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 678 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2150 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 679 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2155 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 682 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2171 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 683 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2176 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Stat' -------------------------------------------------------
data Stat' = Skip' (Int)
           | IfThenElse' (Int) (BExpr) (Stat') (Stat')
           | While' (Int) (BExpr) (Stat')
           | Call' (Int) (Int) (String) (Exprs) (String)
           | IAssign' (Int) (String) (IExpr)
           | BAssign' (Int) (String) (BExpr)
           | Seq' (Stat') (Stat')
           | Malloc' (Int) (String) (IExpr)
           | Free' (Int) (IExpr)
           | RefAssign' (Int) (IExpr) (IExpr)
           | Continue' (Int)
           | Break' (Int)
           deriving ( Show)
-- cata
sem_Stat' :: (Stat') ->
             (T_Stat')
sem_Stat' (Skip' _label) =
    (sem_Stat'_Skip' _label)
sem_Stat' (IfThenElse' _labelc _cond _stat1 _stat2) =
    (sem_Stat'_IfThenElse' _labelc (sem_BExpr _cond) (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (While' _labelc _cond _stat) =
    (sem_Stat'_While' _labelc (sem_BExpr _cond) (sem_Stat' _stat))
sem_Stat' (Call' _labelCall _labelExit _name _params _out) =
    (sem_Stat'_Call' _labelCall _labelExit _name (sem_Exprs _params) _out)
sem_Stat' (IAssign' _label _name _val) =
    (sem_Stat'_IAssign' _label _name (sem_IExpr _val))
sem_Stat' (BAssign' _label _name _val) =
    (sem_Stat'_BAssign' _label _name (sem_BExpr _val))
sem_Stat' (Seq' _stat1 _stat2) =
    (sem_Stat'_Seq' (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (Malloc' _label _name _size) =
    (sem_Stat'_Malloc' _label _name (sem_IExpr _size))
sem_Stat' (Free' _label _ptr) =
    (sem_Stat'_Free' _label (sem_IExpr _ptr))
sem_Stat' (RefAssign' _label _ptr _val) =
    (sem_Stat'_RefAssign' _label (sem_IExpr _ptr) (sem_IExpr _val))
sem_Stat' (Continue' _label) =
    (sem_Stat'_Continue' _label)
sem_Stat' (Break' _label) =
    (sem_Stat'_Break' _label)
-- semantic domain
type T_Stat' = ( M.Map Int String ) ->
               ( M.Map String (Int, Int) ) ->
               ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),Bool,Bool,( M.Map Int Block ),( [Int] ),(M.Map Int (S.Set String)),(M.Map Int (S.Set String)),( [String] ),Stat',([String]))
data Inh_Stat' = Inh_Stat' {labelProcMapPassDown_Inh_Stat' :: ( M.Map Int String ),procMapPassDown_Inh_Stat' :: ( M.Map String (Int, Int) )}
data Syn_Stat' = Syn_Stat' {final_Syn_Stat' :: ([Int]),flow_Syn_Stat' :: ([Flow]),init_Syn_Stat' :: Int,interflow_Syn_Stat' :: ([(Int, Int, Int, Int)]),isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,labelBlockMapCollect_Syn_Stat' :: ( M.Map Int Block ),labels_Syn_Stat' :: ( [Int] ),lvGen_Syn_Stat' :: (M.Map Int (S.Set String)),lvKill_Syn_Stat' :: (M.Map Int (S.Set String)),pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',vars_Syn_Stat' :: ([String])}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIlabelProcMapPassDown _lhsIprocMapPassDown) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars) = sem _lhsIlabelProcMapPassDown _lhsIprocMapPassDown
     in  (Syn_Stat' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOisSingle _lhsOisSkip _lhsOlabelBlockMapCollect _lhsOlabels _lhsOlvGen _lhsOlvKill _lhsOpretty _lhsOself _lhsOvars))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOinit =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2257 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2262 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 219 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2267 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2272 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2277 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 502 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2282 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 565 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2287 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 731 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2292 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 732 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2297 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 733 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2302 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2307 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2312 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _stat1OprocMapPassDown :: ( M.Map String (Int, Int) )
              _stat2OprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _stat1OlabelProcMapPassDown :: ( M.Map Int String )
              _stat2OlabelProcMapPassDown :: ( M.Map Int String )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOself :: Stat'
              _condIfreeVars :: (S.Set String)
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _stat1Ifinal :: ([Int])
              _stat1Iflow :: ([Flow])
              _stat1Iinit :: Int
              _stat1Iinterflow :: ([(Int, Int, Int, Int)])
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1IlabelBlockMapCollect :: ( M.Map Int Block )
              _stat1Ilabels :: ( [Int] )
              _stat1IlvGen :: (M.Map Int (S.Set String))
              _stat1IlvKill :: (M.Map Int (S.Set String))
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1Ivars :: ([String])
              _stat2Ifinal :: ([Int])
              _stat2Iflow :: ([Flow])
              _stat2Iinit :: Int
              _stat2Iinterflow :: ([(Int, Int, Int, Int)])
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2IlabelBlockMapCollect :: ( M.Map Int Block )
              _stat2Ilabels :: ( [Int] )
              _stat2IlvGen :: (M.Map Int (S.Set String))
              _stat2IlvKill :: (M.Map Int (S.Set String))
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2Ivars :: ([String])
              _lhsOinit =
                  ({-# LINE 120 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2377 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 168 "AttributeGrammar.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 2382 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 222 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _stat1Iinit) : Intra (labelc_, _stat2Iinit) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 2387 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 2392 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2397 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2402 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 2407 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 436 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 2412 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 452 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ _condIfreeVars) $ M.union  _stat1IlvGen _stat2IlvGen
                   {-# LINE 2417 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 505 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ (E (B _condIself))) (M.union _stat1IlabelBlockMapCollect _stat2IlabelBlockMapCollect)
                   {-# LINE 2422 "AttributeGrammar.hs" #-}
                   )
              _stat1OlabelProcMapPassDown =
                  ({-# LINE 568 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 2427 "AttributeGrammar.hs" #-}
                   )
              _stat2OlabelProcMapPassDown =
                  ({-# LINE 569 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 2432 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 570 "AttributeGrammar.ag" #-}
                   labelc_ : _stat1Ilabels ++ _stat2Ilabels
                   {-# LINE 2437 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 735 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2448 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 742 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2453 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 743 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2458 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1IlabelBlockMapCollect,_stat1Ilabels,_stat1IlvGen,_stat1IlvKill,_stat1Ipretty,_stat1Iself,_stat1Ivars) =
                  stat1_ _stat1OlabelProcMapPassDown _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2IlabelBlockMapCollect,_stat2Ilabels,_stat2IlvGen,_stat2IlvKill,_stat2Ipretty,_stat2Iself,_stat2Ivars) =
                  stat2_ _stat2OlabelProcMapPassDown _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _statOprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _statOlabelProcMapPassDown :: ( M.Map Int String )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOself :: Stat'
              _condIfreeVars :: (S.Set String)
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _statIfinal :: ([Int])
              _statIflow :: ([Flow])
              _statIinit :: Int
              _statIinterflow :: ([(Int, Int, Int, Int)])
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIlabelBlockMapCollect :: ( M.Map Int Block )
              _statIlabels :: ( [Int] )
              _statIlvGen :: (M.Map Int (S.Set String))
              _statIlvKill :: (M.Map Int (S.Set String))
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIvars :: ([String])
              _lhsOinit =
                  ({-# LINE 123 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2513 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 171 "AttributeGrammar.ag" #-}
                   [labelc_]
                   {-# LINE 2518 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 225 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _statIinit) : map (\x -> Intra (x, labelc_)) _statIfinal ++ _statIflow
                   {-# LINE 2523 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 281 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 2528 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2533 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 374 "AttributeGrammar.ag" #-}
                   _statIvars
                   {-# LINE 2538 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 438 "AttributeGrammar.ag" #-}
                   _statIlvKill
                   {-# LINE 2543 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 454 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ _condIfreeVars) _statIlvGen
                   {-# LINE 2548 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 508 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ (E (B _condIself))) _statIlabelBlockMapCollect
                   {-# LINE 2553 "AttributeGrammar.hs" #-}
                   )
              _statOlabelProcMapPassDown =
                  ({-# LINE 573 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 2558 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 574 "AttributeGrammar.ag" #-}
                   labelc_ : _statIlabels
                   {-# LINE 2563 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 745 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2570 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 748 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2575 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 749 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2580 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              ( _condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlabels,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
                  stat_ _statOlabelProcMapPassDown _statOprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOinit =
                  ({-# LINE 126 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2618 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   [labelExit_]
                   {-# LINE 2623 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 228 "AttributeGrammar.ag" #-}
                   (\(x, y) -> Inter (labelCall_, x) : [Inter (y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 2628 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   (\(x, y) -> [(labelCall_, x, y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 2633 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 379 "AttributeGrammar.ag" #-}
                   [out_]
                   {-# LINE 2638 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 511 "AttributeGrammar.ag" #-}
                   M.singleton labelCall_ (S _self)
                   {-# LINE 2643 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 577 "AttributeGrammar.ag" #-}
                   labelExit_ : [labelCall_]
                   {-# LINE 2648 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 751 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
                   {-# LINE 2653 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 752 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2658 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 753 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2663 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2668 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2673 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelExit_ name_ _paramsIself out_
              _lhsOself =
                  _self
              ( _paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOself :: Stat'
              _valIfreeVars :: (S.Set String)
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOinit =
                  ({-# LINE 129 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2709 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 177 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2714 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 232 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2719 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2724 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 382 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2729 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 440 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ S.singleton name_
                   {-# LINE 2734 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 456 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ _valIfreeVars
                   {-# LINE 2739 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 514 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2744 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 580 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2749 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 755 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2754 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 756 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2759 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 757 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2764 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _valIfreeVars :: (S.Set String)
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOinit =
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2800 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 180 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2805 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 235 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2810 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2815 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 385 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2820 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 517 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2825 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 583 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2830 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 759 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2835 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 760 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2840 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 761 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2845 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2850 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2855 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _stat1OprocMapPassDown :: ( M.Map String (Int, Int) )
              _stat2OprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _stat1OlabelProcMapPassDown :: ( M.Map Int String )
              _stat2OlabelProcMapPassDown :: ( M.Map Int String )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOself :: Stat'
              _stat1Ifinal :: ([Int])
              _stat1Iflow :: ([Flow])
              _stat1Iinit :: Int
              _stat1Iinterflow :: ([(Int, Int, Int, Int)])
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1IlabelBlockMapCollect :: ( M.Map Int Block )
              _stat1Ilabels :: ( [Int] )
              _stat1IlvGen :: (M.Map Int (S.Set String))
              _stat1IlvKill :: (M.Map Int (S.Set String))
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1Ivars :: ([String])
              _stat2Ifinal :: ([Int])
              _stat2Iflow :: ([Flow])
              _stat2Iinit :: Int
              _stat2Iinterflow :: ([(Int, Int, Int, Int)])
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2IlabelBlockMapCollect :: ( M.Map Int Block )
              _stat2Ilabels :: ( [Int] )
              _stat2IlvGen :: (M.Map Int (S.Set String))
              _stat2IlvKill :: (M.Map Int (S.Set String))
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2Ivars :: ([String])
              _lhsOinit =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2916 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 183 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2921 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 238 "AttributeGrammar.ag" #-}
                   _stat1Iflow ++ _stat2Iflow ++ map (\x -> Intra (x, _stat2Iinit)) _stat1Ifinal
                   {-# LINE 2926 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 294 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 2931 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2936 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2941 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 388 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 2946 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 434 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 2951 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 450 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvGen _stat2IlvGen
                   {-# LINE 2956 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 520 "AttributeGrammar.ag" #-}
                   M.union _stat1IlabelBlockMapCollect _stat2IlabelBlockMapCollect
                   {-# LINE 2961 "AttributeGrammar.hs" #-}
                   )
              _stat1OlabelProcMapPassDown =
                  ({-# LINE 586 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 2966 "AttributeGrammar.hs" #-}
                   )
              _stat2OlabelProcMapPassDown =
                  ({-# LINE 587 "AttributeGrammar.ag" #-}
                   _lhsIlabelProcMapPassDown
                   {-# LINE 2971 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 588 "AttributeGrammar.ag" #-}
                   _stat1Ilabels ++ _stat2Ilabels
                   {-# LINE 2976 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 763 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2981 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 764 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2986 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 765 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2991 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1IlabelBlockMapCollect,_stat1Ilabels,_stat1IlvGen,_stat1IlvKill,_stat1Ipretty,_stat1Iself,_stat1Ivars) =
                  stat1_ _stat1OlabelProcMapPassDown _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2IlabelBlockMapCollect,_stat2Ilabels,_stat2IlvGen,_stat2IlvKill,_stat2Ipretty,_stat2Iself,_stat2Ivars) =
                  stat2_ _stat2OlabelProcMapPassDown _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _sizeIfreeVars :: (S.Set String)
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOinit =
                  ({-# LINE 138 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3029 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3034 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 241 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3039 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3044 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 391 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3049 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 523 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3054 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 591 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3059 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 767 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3064 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 768 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3069 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 769 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3074 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3079 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3084 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              ( _sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _ptrIfreeVars :: (S.Set String)
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOinit =
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3119 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 189 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3124 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 244 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3129 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3134 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 394 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3139 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 526 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3144 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 594 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3149 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 771 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3154 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 772 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3159 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 773 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3164 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3169 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3174 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _ptrIfreeVars :: (S.Set String)
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _valIfreeVars :: (S.Set String)
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOinit =
                  ({-# LINE 144 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3214 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 192 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3219 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 247 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3224 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3229 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 397 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3234 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 529 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3239 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 597 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3244 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 775 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3249 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 776 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3254 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 777 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3259 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3264 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3269 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOinit =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3301 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3306 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 250 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3311 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3316 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 400 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3321 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 532 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3326 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 600 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3331 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 779 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3336 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 780 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3341 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 781 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3346 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3351 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3356 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIlabelProcMapPassDown
       _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOlabels :: ( [Int] )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOinit =
                  ({-# LINE 150 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3384 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3389 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3394 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3399 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 403 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3404 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 535 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3409 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 603 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3414 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 783 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3419 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 784 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3424 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 785 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3429 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3434 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3439 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlabels,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))