

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 12 "AttributeGrammar.hs" #-}

{-# LINE 405 "AttributeGrammar.ag" #-}

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

{-# LINE 623 "AttributeGrammar.ag" #-}

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

{-# LINE 718 "AttributeGrammar.ag" #-}

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
             ({-# LINE 749 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 113 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 750 "AttributeGrammar.ag" #-}
              10
              {-# LINE 118 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
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
             ({-# LINE 752 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 140 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 753 "AttributeGrammar.ag" #-}
              10
              {-# LINE 145 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
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
             ({-# LINE 471 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 176 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 755 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 181 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 756 "AttributeGrammar.ag" #-}
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
             ({-# LINE 473 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 216 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 758 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 221 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 759 "AttributeGrammar.ag" #-}
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
             ({-# LINE 475 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 256 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 761 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 261 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 762 "AttributeGrammar.ag" #-}
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
             ({-# LINE 477 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 296 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 764 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 301 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 765 "AttributeGrammar.ag" #-}
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
             ({-# LINE 479 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 336 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 767 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 341 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 768 "AttributeGrammar.ag" #-}
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
             ({-# LINE 770 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 376 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 771 "AttributeGrammar.ag" #-}
              4
              {-# LINE 381 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
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
             ({-# LINE 773 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 416 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 774 "AttributeGrammar.ag" #-}
              3
              {-# LINE 421 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
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
             ({-# LINE 776 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 456 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 777 "AttributeGrammar.ag" #-}
              2
              {-# LINE 461 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
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
             ({-# LINE 779 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 491 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 780 "AttributeGrammar.ag" #-}
              10
              {-# LINE 496 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
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
         _statOprocMapPassDown :: ( M.Map String (Int, Int) )
         _statIfinal :: ([Int])
         _statIflow :: ([Flow])
         _statIinit :: Int
         _statIinterflow :: ([(Int, Int, Int, Int)])
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIlabelBlockMapCollect :: ( M.Map Int Block )
         _statIlvGen :: (M.Map Int (S.Set String))
         _statIlvKill :: (M.Map Int (S.Set String))
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _statIvars :: ([String])
         _self =
             S _statIself
         _lhsOself =
             _self
         _statOprocMapPassDown =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
              error "missing rule: Block.S.stat.procMapPassDown"
              {-# LINE 567 "AttributeGrammar.hs" #-}
              )
         ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
             stat_ _statOprocMapPassDown
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
         _stat'OprocMapPassDown :: ( M.Map String (Int, Int) )
         _stat'Ifinal :: ([Int])
         _stat'Iflow :: ([Flow])
         _stat'Iinit :: Int
         _stat'Iinterflow :: ([(Int, Int, Int, Int)])
         _stat'IisSingle :: Bool
         _stat'IisSkip :: Bool
         _stat'IlabelBlockMapCollect :: ( M.Map Int Block )
         _stat'IlvGen :: (M.Map Int (S.Set String))
         _stat'IlvKill :: (M.Map Int (S.Set String))
         _stat'Ipretty :: ( [String] )
         _stat'Iself :: Stat'
         _stat'Ivars :: ([String])
         _self =
             CStat _stat'Iself
         _lhsOself =
             _self
         _stat'OprocMapPassDown =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CStat.stat'.procMapPassDown"
              {-# LINE 655 "AttributeGrammar.hs" #-}
              )
         ( _stat'Ifinal,_stat'Iflow,_stat'Iinit,_stat'Iinterflow,_stat'IisSingle,_stat'IisSkip,_stat'IlabelBlockMapCollect,_stat'IlvGen,_stat'IlvKill,_stat'Ipretty,_stat'Iself,_stat'Ivars) =
             stat'_ _stat'OprocMapPassDown
     in  ( _lhsOself))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let _lhsOself :: Code
         _proc'OprocMapPassDown :: ( M.Map String (Int, Int) )
         _proc'Ifinal :: ([Int])
         _proc'Iflow :: ([Flow])
         _proc'Iinit :: Int
         _proc'Iinterflow :: ([(Int, Int, Int, Int)])
         _proc'IlabelBlockMapCollect :: ( M.Map Int Block )
         _proc'Ipretty :: ( [String] )
         _proc'IprocMapCollect :: ( M.Map String (Int, Int) )
         _proc'Iself :: Proc'
         _proc'Ivars :: ([String])
         _self =
             CProc _proc'Iself
         _lhsOself =
             _self
         _proc'OprocMapPassDown =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CProc.proc'.procMapPassDown"
              {-# LINE 681 "AttributeGrammar.hs" #-}
              )
         ( _proc'Ifinal,_proc'Iflow,_proc'Iinit,_proc'Iinterflow,_proc'IlabelBlockMapCollect,_proc'Ipretty,_proc'IprocMapCollect,_proc'Iself,_proc'Ivars) =
             proc'_ _proc'OprocMapPassDown
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
         ( _program'Ifinal,_program'Iflow,_program'Iinit,_program'Iinterflow,_program'IlabelBlockMapCollect,_program'IlvGen,_program'IlvKill,_program'IlvLambda,_program'Ipretty,_program'IprocMapCollect,_program'Iself,_program'Ivars) =
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
             ({-# LINE 784 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 742 "AttributeGrammar.hs" #-}
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
             ({-# LINE 786 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 763 "AttributeGrammar.hs" #-}
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
             ({-# LINE 792 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 802 "AttributeGrammar.hs" #-}
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
             ({-# LINE 790 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 820 "AttributeGrammar.hs" #-}
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
             ({-# LINE 726 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 911 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 727 "AttributeGrammar.ag" #-}
              10
              {-# LINE 916 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 921 "AttributeGrammar.hs" #-}
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
             ({-# LINE 459 "AttributeGrammar.ag" #-}
              S.singleton name_
              {-# LINE 938 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 729 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 943 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 730 "AttributeGrammar.ag" #-}
              10
              {-# LINE 948 "AttributeGrammar.hs" #-}
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
             ({-# LINE 461 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 974 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 732 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 979 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 733 "AttributeGrammar.ag" #-}
              6
              {-# LINE 984 "AttributeGrammar.hs" #-}
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
             ({-# LINE 463 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 1014 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 735 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 1019 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 736 "AttributeGrammar.ag" #-}
              6
              {-# LINE 1024 "AttributeGrammar.hs" #-}
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
             ({-# LINE 465 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 1054 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 738 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1059 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 739 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1064 "AttributeGrammar.hs" #-}
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
             ({-# LINE 467 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 1094 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 741 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1099 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 742 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1104 "AttributeGrammar.hs" #-}
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
             ({-# LINE 744 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1129 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 745 "AttributeGrammar.ag" #-}
              10
              {-# LINE 1134 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 1139 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 559 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1184 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 560 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 1189 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 561 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 1194 "AttributeGrammar.hs" #-}
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
type T_Proc' = ( M.Map String (Int, Int) ) ->
               ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),( M.Map Int Block ),( [String] ),( M.Map String (Int, Int) ),Proc',([String]))
data Inh_Proc' = Inh_Proc' {procMapPassDown_Inh_Proc' :: ( M.Map String (Int, Int) )}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ([Int]),flow_Syn_Proc' :: ([Flow]),init_Syn_Proc' :: Int,interflow_Syn_Proc' :: ([(Int, Int, Int, Int)]),labelBlockMapCollect_Syn_Proc' :: ( M.Map Int Block ),pretty_Syn_Proc' :: ( [String] ),procMapCollect_Syn_Proc' :: ( M.Map String (Int, Int) ),self_Syn_Proc' :: Proc',vars_Syn_Proc' :: ([String])}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIprocMapPassDown) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars) = sem _lhsIprocMapPassDown
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOlabelBlockMapCollect _lhsOpretty _lhsOprocMapCollect _lhsOself _lhsOvars))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelReturn_ name_ inp_ out_ stat_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
              _statOprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOself :: Proc'
              _statIfinal :: ([Int])
              _statIflow :: ([Flow])
              _statIinit :: Int
              _statIinterflow :: ([(Int, Int, Int, Int)])
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIlabelBlockMapCollect :: ( M.Map Int Block )
              _statIlvGen :: (M.Map Int (S.Set String))
              _statIlvKill :: (M.Map Int (S.Set String))
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIvars :: ([String])
              _lhsOinit =
                  ({-# LINE 113 "AttributeGrammar.ag" #-}
                   labelEntry_
                   {-# LINE 1256 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   [labelReturn_]
                   {-# LINE 1261 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 209 "AttributeGrammar.ag" #-}
                   _statIflow
                   {-# LINE 1266 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 265 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1271 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   M.singleton name_ (labelEntry_, labelReturn_)
                   {-# LINE 1276 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1281 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 358 "AttributeGrammar.ag" #-}
                   L.nub (map (\x -> name_ ++ x) _statIvars)
                   {-# LINE 1286 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 491 "AttributeGrammar.ag" #-}
                   _statIlabelBlockMapCollect
                   {-# LINE 1291 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 656 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelReturn_ ++ ";"]
                   {-# LINE 1298 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelReturn_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
                  stat_ _statOprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars)))
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
                  ({-# LINE 555 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1344 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 538 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1353 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 538 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1358 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 538 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1363 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 553 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1379 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 538 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1388 "AttributeGrammar.hs" #-}
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
type T_Procs' = ( M.Map String (Int, Int) ) ->
                ( ([Flow]),([(Int, Int, Int, Int)]),( M.Map Int Block ),( [String] ),( M.Map String (Int, Int) ),Procs',([String]))
data Inh_Procs' = Inh_Procs' {procMapPassDown_Inh_Procs' :: ( M.Map String (Int, Int) )}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ([Flow]),interflow_Syn_Procs' :: ([(Int, Int, Int, Int)]),labelBlockMapCollect_Syn_Procs' :: ( M.Map Int Block ),pretty_Syn_Procs' :: ( [String] ),procMapCollect_Syn_Procs' :: ( M.Map String (Int, Int) ),self_Syn_Procs' :: Procs',vars_Syn_Procs' :: ([String])}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIprocMapPassDown) =
    (let ( _lhsOflow,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars) = sem _lhsIprocMapPassDown
     in  (Syn_Procs' _lhsOflow _lhsOinterflow _lhsOlabelBlockMapCollect _lhsOpretty _lhsOprocMapCollect _lhsOself _lhsOvars))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
              _hdOprocMapPassDown :: ( M.Map String (Int, Int) )
              _tlOprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOself :: Procs'
              _hdIfinal :: ([Int])
              _hdIflow :: ([Flow])
              _hdIinit :: Int
              _hdIinterflow :: ([(Int, Int, Int, Int)])
              _hdIlabelBlockMapCollect :: ( M.Map Int Block )
              _hdIpretty :: ( [String] )
              _hdIprocMapCollect :: ( M.Map String (Int, Int) )
              _hdIself :: Proc'
              _hdIvars :: ([String])
              _tlIflow :: ([Flow])
              _tlIinterflow :: ([(Int, Int, Int, Int)])
              _tlIlabelBlockMapCollect :: ( M.Map Int Block )
              _tlIpretty :: ( [String] )
              _tlIprocMapCollect :: ( M.Map String (Int, Int) )
              _tlIself :: Procs'
              _tlIvars :: ([String])
              _lhsOflow =
                  ({-# LINE 215 "AttributeGrammar.ag" #-}
                   _hdIflow ++ _tlIflow
                   {-# LINE 1442 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 271 "AttributeGrammar.ag" #-}
                   _hdIinterflow ++ _tlIinterflow
                   {-# LINE 1447 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   M.union _hdIprocMapCollect _tlIprocMapCollect
                   {-# LINE 1452 "AttributeGrammar.hs" #-}
                   )
              _hdOprocMapPassDown =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1457 "AttributeGrammar.hs" #-}
                   )
              _tlOprocMapPassDown =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1462 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   L.nub (_hdIvars ++ _tlIvars)
                   {-# LINE 1467 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 497 "AttributeGrammar.ag" #-}
                   M.union _hdIlabelBlockMapCollect _tlIlabelBlockMapCollect
                   {-# LINE 1472 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 652 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1477 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIinterflow,_hdIlabelBlockMapCollect,_hdIpretty,_hdIprocMapCollect,_hdIself,_hdIvars) =
                  hd_ _hdOprocMapPassDown
              ( _tlIflow,_tlIinterflow,_tlIlabelBlockMapCollect,_tlIpretty,_tlIprocMapCollect,_tlIself,_tlIvars) =
                  tl_ _tlOprocMapPassDown
          in  ( _lhsOflow,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOself :: Procs'
              _lhsOflow =
                  ({-# LINE 213 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1501 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1506 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1511 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1516 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 495 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1521 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 650 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1526 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOflow,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars)))
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
             ({-# LINE 547 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1568 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 548 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1573 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 549 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 1578 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),( M.Map Int Block ),(M.Map Int (S.Set String)),(M.Map Int (S.Set String)),( Int -> (LVSet-> LVSet) ),String,( M.Map String (Int, Int) ),Program',([String]))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ([Int]),flow_Syn_Program' :: ([Flow]),init_Syn_Program' :: Int,interflow_Syn_Program' :: ([(Int, Int, Int, Int)]),labelBlockMapCollect_Syn_Program' :: ( M.Map Int Block ),lvGen_Syn_Program' :: (M.Map Int (S.Set String)),lvKill_Syn_Program' :: (M.Map Int (S.Set String)),lvLambda_Syn_Program' :: ( Int -> (LVSet-> LVSet) ),pretty_Syn_Program' :: String,procMapCollect_Syn_Program' :: ( M.Map String (Int, Int) ),self_Syn_Program' :: Program',vars_Syn_Program' :: ([String])}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOlvLambda,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOlabelBlockMapCollect _lhsOlvGen _lhsOlvKill _lhsOlvLambda _lhsOpretty _lhsOprocMapCollect _lhsOself _lhsOvars))
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
         _lhsOpretty :: String
         _lhsOself :: Program'
         _procsIflow :: ([Flow])
         _procsIinterflow :: ([(Int, Int, Int, Int)])
         _procsIlabelBlockMapCollect :: ( M.Map Int Block )
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
         _statIlvGen :: (M.Map Int (S.Set String))
         _statIlvKill :: (M.Map Int (S.Set String))
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _statIvars :: ([String])
         _lhsOinit =
             ({-# LINE 109 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1647 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 157 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1652 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 205 "AttributeGrammar.ag" #-}
              _statIflow ++ _procsIflow
              {-# LINE 1657 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 261 "AttributeGrammar.ag" #-}
              _statIinterflow ++ _procsIinterflow
              {-# LINE 1662 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 318 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1667 "AttributeGrammar.hs" #-}
              )
         _statOprocMapPassDown =
             ({-# LINE 319 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1672 "AttributeGrammar.hs" #-}
              )
         _procsOprocMapPassDown =
             ({-# LINE 320 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1677 "AttributeGrammar.hs" #-}
              )
         _lhsOvars =
             ({-# LINE 353 "AttributeGrammar.ag" #-}
              L.nub (_statIvars ++ _procsIvars)
              {-# LINE 1682 "AttributeGrammar.hs" #-}
              )
         _lhsOlvLambda =
             ({-# LINE 424 "AttributeGrammar.ag" #-}
              genLambda _statIlvGen _statIlvKill
              {-# LINE 1687 "AttributeGrammar.hs" #-}
              )
         _lhsOlvKill =
             ({-# LINE 429 "AttributeGrammar.ag" #-}
              _statIlvKill
              {-# LINE 1692 "AttributeGrammar.hs" #-}
              )
         _lhsOlvGen =
             ({-# LINE 445 "AttributeGrammar.ag" #-}
              _statIlvGen
              {-# LINE 1697 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelBlockMapCollect =
             ({-# LINE 487 "AttributeGrammar.ag" #-}
              M.union _procsIlabelBlockMapCollect _statIlabelBlockMapCollect
              {-# LINE 1702 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 646 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1707 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself
         _lhsOself =
             _self
         ( _procsIflow,_procsIinterflow,_procsIlabelBlockMapCollect,_procsIpretty,_procsIprocMapCollect,_procsIself,_procsIvars) =
             procs_ _procsOprocMapPassDown
         ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
             stat_ _statOprocMapPassDown
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOlvLambda,_lhsOpretty,_lhsOprocMapCollect,_lhsOself,_lhsOvars))
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
                  ({-# LINE 565 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1779 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 566 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1784 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 569 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1811 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 570 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1816 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 571 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1821 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 572 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1826 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 575 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1852 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 576 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1857 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 577 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1862 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 580 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1883 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 581 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1888 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 584 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1906 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 585 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1911 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 588 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1929 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 589 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1934 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 592 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1960 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 593 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1965 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 594 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1970 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 538 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1979 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 597 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1997 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 598 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 2002 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 601 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2019 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 602 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 2024 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 605 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2042 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 606 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 2047 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 609 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2063 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 610 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2068 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 613 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2084 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 614 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2089 "AttributeGrammar.hs" #-}
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
type T_Stat' = ( M.Map String (Int, Int) ) ->
               ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),Bool,Bool,( M.Map Int Block ),(M.Map Int (S.Set String)),(M.Map Int (S.Set String)),( [String] ),Stat',([String]))
data Inh_Stat' = Inh_Stat' {procMapPassDown_Inh_Stat' :: ( M.Map String (Int, Int) )}
data Syn_Stat' = Syn_Stat' {final_Syn_Stat' :: ([Int]),flow_Syn_Stat' :: ([Flow]),init_Syn_Stat' :: Int,interflow_Syn_Stat' :: ([(Int, Int, Int, Int)]),isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,labelBlockMapCollect_Syn_Stat' :: ( M.Map Int Block ),lvGen_Syn_Stat' :: (M.Map Int (S.Set String)),lvKill_Syn_Stat' :: (M.Map Int (S.Set String)),pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',vars_Syn_Stat' :: ([String])}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIprocMapPassDown) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars) = sem _lhsIprocMapPassDown
     in  (Syn_Stat' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOisSingle _lhsOisSkip _lhsOlabelBlockMapCollect _lhsOlvGen _lhsOlvKill _lhsOpretty _lhsOself _lhsOvars))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOinit =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2167 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2172 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 219 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2177 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2182 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2187 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 501 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2192 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 662 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2197 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 663 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2202 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 664 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2207 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2212 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Skip'.lhs.lvGen"
                   {-# LINE 2221 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIprocMapPassDown ->
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
              _stat2IlvGen :: (M.Map Int (S.Set String))
              _stat2IlvKill :: (M.Map Int (S.Set String))
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2Ivars :: ([String])
              _lhsOinit =
                  ({-# LINE 120 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2276 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 168 "AttributeGrammar.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 2281 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 222 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _stat1Iinit) : Intra (labelc_, _stat2Iinit) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 2286 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 2291 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2296 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2301 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 2306 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 435 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 2311 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 451 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ _condIfreeVars) $ M.union  _stat1IlvGen _stat2IlvGen
                   {-# LINE 2316 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 504 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ (E (B _condIself))) (M.union _stat1IlabelBlockMapCollect _stat2IlabelBlockMapCollect)
                   {-# LINE 2321 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 666 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2332 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 673 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2337 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 674 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2342 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1IlabelBlockMapCollect,_stat1IlvGen,_stat1IlvKill,_stat1Ipretty,_stat1Iself,_stat1Ivars) =
                  stat1_ _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2IlabelBlockMapCollect,_stat2IlvGen,_stat2IlvKill,_stat2Ipretty,_stat2Iself,_stat2Ivars) =
                  stat2_ _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _statOprocMapPassDown :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
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
              _statIlvGen :: (M.Map Int (S.Set String))
              _statIlvKill :: (M.Map Int (S.Set String))
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIvars :: ([String])
              _lhsOinit =
                  ({-# LINE 123 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2393 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 171 "AttributeGrammar.ag" #-}
                   [labelc_]
                   {-# LINE 2398 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 225 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _statIinit) : map (\x -> Intra (x, labelc_)) _statIfinal ++ _statIflow
                   {-# LINE 2403 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 281 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 2408 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2413 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 374 "AttributeGrammar.ag" #-}
                   _statIvars
                   {-# LINE 2418 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 437 "AttributeGrammar.ag" #-}
                   _statIlvKill
                   {-# LINE 2423 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 453 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ _condIfreeVars) _statIlvGen
                   {-# LINE 2428 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 507 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ (E (B _condIself))) _statIlabelBlockMapCollect
                   {-# LINE 2433 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 676 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2440 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 679 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2445 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 680 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2450 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              ( _condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlabelBlockMapCollect,_statIlvGen,_statIlvKill,_statIpretty,_statIself,_statIvars) =
                  stat_ _statOprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOinit =
                  ({-# LINE 126 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2486 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   [labelExit_]
                   {-# LINE 2491 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 228 "AttributeGrammar.ag" #-}
                   (\(x, y) -> Inter (labelCall_, x) : [Inter (y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 2496 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   (\(x, y) -> [(labelCall_, x, y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 2501 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 378 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2506 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 510 "AttributeGrammar.ag" #-}
                   undefined
                   {-# LINE 2511 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 682 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
                   {-# LINE 2516 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 683 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2521 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 684 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2526 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2531 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelExit_ name_ _paramsIself out_
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Call'.lhs.lvGen"
                   {-# LINE 2540 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
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
                   {-# LINE 2570 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 177 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2575 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 232 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2580 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2585 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 381 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2590 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 439 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ S.singleton name_
                   {-# LINE 2595 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 455 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ _valIfreeVars
                   {-# LINE 2600 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 513 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2605 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 686 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2610 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 687 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2615 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 688 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2620 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _valIfreeVars :: (S.Set String)
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOinit =
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2654 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 180 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2659 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 235 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2664 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2669 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 384 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2674 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 516 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2679 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 690 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2684 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 691 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2689 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 692 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2694 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2699 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.BAssign'.lhs.lvGen"
                   {-# LINE 2708 "AttributeGrammar.hs" #-}
                   )
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIprocMapPassDown ->
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
              _stat2IlvGen :: (M.Map Int (S.Set String))
              _stat2IlvKill :: (M.Map Int (S.Set String))
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2Ivars :: ([String])
              _lhsOinit =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2759 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 183 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2764 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 238 "AttributeGrammar.ag" #-}
                   _stat1Iflow ++ _stat2Iflow ++ map (\x -> Intra (x, _stat2Iinit)) _stat1Ifinal
                   {-# LINE 2769 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 294 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 2774 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2779 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2784 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 387 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 2789 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 433 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 2794 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 449 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvGen _stat2IlvGen
                   {-# LINE 2799 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 519 "AttributeGrammar.ag" #-}
                   M.union _stat1IlabelBlockMapCollect _stat2IlabelBlockMapCollect
                   {-# LINE 2804 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 694 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2809 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 695 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2814 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 696 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2819 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1IlabelBlockMapCollect,_stat1IlvGen,_stat1IlvKill,_stat1Ipretty,_stat1Iself,_stat1Ivars) =
                  stat1_ _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2IlabelBlockMapCollect,_stat2IlvGen,_stat2IlvKill,_stat2Ipretty,_stat2Iself,_stat2Ivars) =
                  stat2_ _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _sizeIfreeVars :: (S.Set String)
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOinit =
                  ({-# LINE 138 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2855 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2860 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 241 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2865 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2870 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 390 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2875 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 522 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2880 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 698 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2885 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 699 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2890 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 700 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2895 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2900 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Malloc'.lhs.lvGen"
                   {-# LINE 2909 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _ptrIfreeVars :: (S.Set String)
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOinit =
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2938 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 189 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2943 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 244 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2948 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2953 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 393 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2958 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 525 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 2963 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 702 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2968 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 703 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2973 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 704 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2978 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2983 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Free'.lhs.lvGen"
                   {-# LINE 2992 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
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
                   {-# LINE 3026 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 192 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3031 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 247 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3036 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3041 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 396 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3046 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 528 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3051 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 706 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3056 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 707 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3061 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 708 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3066 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3071 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.RefAssign'.lhs.lvGen"
                   {-# LINE 3080 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOinit =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3106 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3111 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 250 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3116 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3121 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 399 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3126 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 531 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3131 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 710 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3136 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 711 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3141 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 712 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3146 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3151 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Continue'.lhs.lvGen"
                   {-# LINE 3160 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOlabelBlockMapCollect :: ( M.Map Int Block )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOself :: Stat'
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOinit =
                  ({-# LINE 150 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3182 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 3187 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3192 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3197 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 402 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 3202 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelBlockMapCollect =
                  ({-# LINE 534 "AttributeGrammar.ag" #-}
                   M.singleton label_ (S _self)
                   {-# LINE 3207 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 714 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3212 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 715 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3217 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 716 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3222 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3227 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
              _lhsOlvGen =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Break'.lhs.lvGen"
                   {-# LINE 3236 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlabelBlockMapCollect,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOself,_lhsOvars)))