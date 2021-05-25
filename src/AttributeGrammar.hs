

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 12 "AttributeGrammar.hs" #-}

{-# LINE 402 "AttributeGrammar.ag" #-}

genLambda :: M.Map Int (S.Set String) -> M.Map Int (S.Set String) -> Int -> (S.Set String -> S.Set String)
genLambda lvGen lvKill l  = (S.union $ recklessLookup l lvGen) . (S.intersection (recklessLookup l lvKill))
  where
    recklessLookup k m = case M.lookup k m of
      Nothing -> S.empty 
      Just n -> n
{-# LINE 22 "AttributeGrammar.hs" #-}

{-# LINE 551 "AttributeGrammar.ag" #-}

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
{-# LINE 44 "AttributeGrammar.hs" #-}

{-# LINE 646 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 51 "AttributeGrammar.hs" #-}
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
type T_BExpr = ( Int,String)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOprecedence,_lhsOpretty) = sem
     in  (Syn_BExpr _lhsOprecedence _lhsOpretty))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 677 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 108 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 678 "AttributeGrammar.ag" #-}
              10
              {-# LINE 113 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 680 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 124 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 681 "AttributeGrammar.ag" #-}
              10
              {-# LINE 129 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 683 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 147 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 684 "AttributeGrammar.ag" #-}
              4
              {-# LINE 152 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 686 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 174 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 687 "AttributeGrammar.ag" #-}
              4
              {-# LINE 179 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 689 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 201 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 690 "AttributeGrammar.ag" #-}
              4
              {-# LINE 206 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 692 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 228 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 693 "AttributeGrammar.ag" #-}
              4
              {-# LINE 233 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 695 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 255 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 696 "AttributeGrammar.ag" #-}
              4
              {-# LINE 260 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 698 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 280 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 699 "AttributeGrammar.ag" #-}
              4
              {-# LINE 285 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 701 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 305 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 702 "AttributeGrammar.ag" #-}
              3
              {-# LINE 310 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 704 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 330 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 705 "AttributeGrammar.ag" #-}
              2
              {-# LINE 335 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOpretty =
             ({-# LINE 707 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 352 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 708 "AttributeGrammar.ag" #-}
              10
              {-# LINE 357 "AttributeGrammar.hs" #-}
              )
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOprecedence,_lhsOpretty))
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
type T_Code = ( )
data Inh_Code = Inh_Code {}
data Syn_Code = Syn_Code {}
wrap_Code :: T_Code ->
             Inh_Code ->
             Syn_Code
wrap_Code sem (Inh_Code) =
    (let ( ) = sem
     in  (Syn_Code))
sem_Code_CBExpr :: T_BExpr ->
                   T_Code
sem_Code_CBExpr bExpr_ =
    (let _bExprIprecedence :: Int
         _bExprIpretty :: String
         ( _bExprIprecedence,_bExprIpretty) =
             bExpr_
     in  ( ))
sem_Code_CIExpr :: T_IExpr ->
                   T_Code
sem_Code_CIExpr iExpr_ =
    (let _iExprIfreeVars :: (S.Set String)
         _iExprIprecedence :: Int
         _iExprIpretty :: String
         ( _iExprIfreeVars,_iExprIprecedence,_iExprIpretty) =
             iExpr_
     in  ( ))
sem_Code_CStat :: (T_Stat') ->
                  T_Code
sem_Code_CStat stat'_ =
    (let _stat'OprocMapPassDown :: ( M.Map String (Int, Int) )
         _stat'Ifinal :: ([Int])
         _stat'Iflow :: ([Flow])
         _stat'Iinit :: Int
         _stat'Iinterflow :: ([(Int, Int, Int, Int)])
         _stat'IisSingle :: Bool
         _stat'IisSkip :: Bool
         _stat'IlvGen :: (M.Map Int (S.Set String))
         _stat'IlvKill :: (M.Map Int (S.Set String))
         _stat'Ipretty :: ( [String] )
         _stat'Ivars :: ([String])
         _stat'OprocMapPassDown =
             ({-# LINE 311 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CStat.stat'.procMapPassDown"
              {-# LINE 425 "AttributeGrammar.hs" #-}
              )
         ( _stat'Ifinal,_stat'Iflow,_stat'Iinit,_stat'Iinterflow,_stat'IisSingle,_stat'IisSkip,_stat'IlvGen,_stat'IlvKill,_stat'Ipretty,_stat'Ivars) =
             stat'_ _stat'OprocMapPassDown
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let _proc'OprocMapPassDown :: ( M.Map String (Int, Int) )
         _proc'Ifinal :: ([Int])
         _proc'Iflow :: ([Flow])
         _proc'Iinit :: Int
         _proc'Iinterflow :: ([(Int, Int, Int, Int)])
         _proc'Ipretty :: ( [String] )
         _proc'IprocMapCollect :: ( M.Map String (Int, Int) )
         _proc'Ivars :: ([String])
         _proc'OprocMapPassDown =
             ({-# LINE 311 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CProc.proc'.procMapPassDown"
              {-# LINE 444 "AttributeGrammar.hs" #-}
              )
         ( _proc'Ifinal,_proc'Iflow,_proc'Iinit,_proc'Iinterflow,_proc'Ipretty,_proc'IprocMapCollect,_proc'Ivars) =
             proc'_ _proc'OprocMapPassDown
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let _program'Ifinal :: ([Int])
         _program'Iflow :: ([Flow])
         _program'Iinit :: Int
         _program'Iinterflow :: ([(Int, Int, Int, Int)])
         _program'IlvGen :: (M.Map Int (S.Set String))
         _program'IlvKill :: (M.Map Int (S.Set String))
         _program'IlvLambda :: ( Int -> (S.Set String -> S.Set String) )
         _program'Ipretty :: String
         _program'IprocMapCollect :: ( M.Map String (Int, Int) )
         _program'Ivars :: ([String])
         ( _program'Ifinal,_program'Iflow,_program'Iinit,_program'Iinterflow,_program'IlvGen,_program'IlvKill,_program'IlvLambda,_program'Ipretty,_program'IprocMapCollect,_program'Ivars) =
             program'_
     in  ( ))
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
type T_Expr = ( String)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {pretty_Syn_Expr :: String}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOpretty) = sem
     in  (Syn_Expr _lhsOpretty))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B expr_ =
    (let _lhsOpretty :: String
         _exprIprecedence :: Int
         _exprIpretty :: String
         _lhsOpretty =
             ({-# LINE 712 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 495 "AttributeGrammar.hs" #-}
              )
         ( _exprIprecedence,_exprIpretty) =
             expr_
     in  ( _lhsOpretty))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOpretty :: String
         _exprIfreeVars :: (S.Set String)
         _exprIprecedence :: Int
         _exprIpretty :: String
         _lhsOpretty =
             ({-# LINE 714 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 510 "AttributeGrammar.hs" #-}
              )
         ( _exprIfreeVars,_exprIprecedence,_exprIpretty) =
             expr_
     in  ( _lhsOpretty))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( String)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {pretty_Syn_Exprs :: String}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOpretty) = sem
     in  (Syn_Exprs _lhsOpretty))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOpretty :: String
         _hdIpretty :: String
         _tlIpretty :: String
         _lhsOpretty =
             ({-# LINE 720 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 542 "AttributeGrammar.hs" #-}
              )
         ( _hdIpretty) =
             hd_
         ( _tlIpretty) =
             tl_
     in  ( _lhsOpretty))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOpretty :: String
         _lhsOpretty =
             ({-# LINE 718 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 555 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOpretty))
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
type T_Flow = ( )
data Inh_Flow = Inh_Flow {}
data Syn_Flow = Syn_Flow {}
wrap_Flow :: T_Flow ->
             Inh_Flow ->
             Syn_Flow
wrap_Flow sem (Inh_Flow) =
    (let ( ) = sem
     in  (Syn_Flow))
sem_Flow_Intra :: ((Int, Int)) ->
                  T_Flow
sem_Flow_Intra labels_ =
    (let
     in  ( ))
sem_Flow_Inter :: ((Int, Int)) ->
                  T_Flow
sem_Flow_Inter labels_ =
    (let
     in  ( ))
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
type T_IExpr = ( (S.Set String),Int,String)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {freeVars_Syn_IExpr :: (S.Set String),precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty) = sem
     in  (Syn_IExpr _lhsOfreeVars _lhsOprecedence _lhsOpretty))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOpretty =
             ({-# LINE 654 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 633 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 655 "AttributeGrammar.ag" #-}
              10
              {-# LINE 638 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 643 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars =
             ({-# LINE 453 "AttributeGrammar.ag" #-}
              S.singleton name_
              {-# LINE 655 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 657 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 660 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 658 "AttributeGrammar.ag" #-}
              10
              {-# LINE 665 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOfreeVars =
             ({-# LINE 455 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 684 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 660 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 689 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 661 "AttributeGrammar.ag" #-}
              6
              {-# LINE 694 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOfreeVars =
             ({-# LINE 457 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 717 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 663 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 722 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 664 "AttributeGrammar.ag" #-}
              6
              {-# LINE 727 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOfreeVars =
             ({-# LINE 459 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 750 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 666 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 755 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 667 "AttributeGrammar.ag" #-}
              7
              {-# LINE 760 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOfreeVars =
             ({-# LINE 461 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 783 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 669 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 788 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 670 "AttributeGrammar.ag" #-}
              7
              {-# LINE 793 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _ptrIfreeVars :: (S.Set String)
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _lhsOpretty =
             ({-# LINE 672 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 812 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 673 "AttributeGrammar.ag" #-}
              10
              {-# LINE 817 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 822 "AttributeGrammar.hs" #-}
              )
         ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty) =
             ptr_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
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
              ( Int,Proc')
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Int}
data Syn_Proc = Syn_Proc {label_Syn_Proc :: Int,labelled_Syn_Proc :: Proc'}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled) = sem _lhsIlabel
     in  (Syn_Proc _lhsOlabel _lhsOlabelled))
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
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statOlabel =
                  ({-# LINE 487 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 861 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 488 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 866 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 489 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 871 "AttributeGrammar.hs" #-}
                   )
              ( _statIlabel,_statIlabelled) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOlabelled)))
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
               ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),( [String] ),( M.Map String (Int, Int) ),([String]))
data Inh_Proc' = Inh_Proc' {procMapPassDown_Inh_Proc' :: ( M.Map String (Int, Int) )}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ([Int]),flow_Syn_Proc' :: ([Flow]),init_Syn_Proc' :: Int,interflow_Syn_Proc' :: ([(Int, Int, Int, Int)]),pretty_Syn_Proc' :: ( [String] ),procMapCollect_Syn_Proc' :: ( M.Map String (Int, Int) ),vars_Syn_Proc' :: ([String])}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIprocMapPassDown) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOprocMapCollect,_lhsOvars) = sem _lhsIprocMapPassDown
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOpretty _lhsOprocMapCollect _lhsOvars))
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
              _lhsOpretty :: ( [String] )
              _statIfinal :: ([Int])
              _statIflow :: ([Flow])
              _statIinit :: Int
              _statIinterflow :: ([(Int, Int, Int, Int)])
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIlvGen :: (M.Map Int (S.Set String))
              _statIlvKill :: (M.Map Int (S.Set String))
              _statIpretty :: ( [String] )
              _statIvars :: ([String])
              _lhsOinit =
                  ({-# LINE 110 "AttributeGrammar.ag" #-}
                   labelEntry_
                   {-# LINE 925 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   [labelReturn_]
                   {-# LINE 930 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _statIflow
                   {-# LINE 935 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 262 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 940 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 321 "AttributeGrammar.ag" #-}
                   M.singleton name_ (labelEntry_, labelReturn_)
                   {-# LINE 945 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 950 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   L.nub (map (\x -> name_ ++ x) _statIvars)
                   {-# LINE 955 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 584 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelReturn_ ++ ";"]
                   {-# LINE 962 "AttributeGrammar.hs" #-}
                   )
              ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlvGen,_statIlvKill,_statIpretty,_statIvars) =
                  stat_ _statOprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOprocMapCollect,_lhsOvars)))
-- Procs -------------------------------------------------------
type Procs = [Proc]
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs list =
    (Prelude.foldr sem_Procs_Cons sem_Procs_Nil (Prelude.map sem_Proc list))
-- semantic domain
type T_Procs = Int ->
               ( Int,Procs')
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Int}
data Syn_Procs = Syn_Procs {label_Syn_Procs :: Int,labelled_Syn_Procs :: Procs'}
wrap_Procs :: T_Procs ->
              Inh_Procs ->
              Syn_Procs
wrap_Procs sem (Inh_Procs _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled) = sem _lhsIlabel
     in  (Syn_Procs _lhsOlabel _lhsOlabelled))
sem_Procs_Cons :: T_Proc ->
                  T_Procs ->
                  T_Procs
sem_Procs_Cons hd_ tl_ =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOlabel :: Int
              _hdOlabel :: Int
              _tlOlabel :: Int
              _hdIlabel :: Int
              _hdIlabelled :: Proc'
              _tlIlabel :: Int
              _tlIlabelled :: Procs'
              _lhsOlabelled =
                  ({-# LINE 483 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1001 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 466 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1006 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 466 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1011 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 466 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1016 "AttributeGrammar.hs" #-}
                   )
              ( _hdIlabel,_hdIlabelled) =
                  hd_ _hdOlabel
              ( _tlIlabel,_tlIlabelled) =
                  tl_ _tlOlabel
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOlabel :: Int
              _lhsOlabelled =
                  ({-# LINE 481 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1031 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 466 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1036 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
-- Procs' ------------------------------------------------------
type Procs' = [Proc']
-- cata
sem_Procs' :: (Procs') ->
              (T_Procs')
sem_Procs' list =
    (Prelude.foldr sem_Procs'_Cons sem_Procs'_Nil (Prelude.map sem_Proc' list))
-- semantic domain
type T_Procs' = ( M.Map String (Int, Int) ) ->
                ( ([Flow]),([(Int, Int, Int, Int)]),( [String] ),( M.Map String (Int, Int) ),([String]))
data Inh_Procs' = Inh_Procs' {procMapPassDown_Inh_Procs' :: ( M.Map String (Int, Int) )}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ([Flow]),interflow_Syn_Procs' :: ([(Int, Int, Int, Int)]),pretty_Syn_Procs' :: ( [String] ),procMapCollect_Syn_Procs' :: ( M.Map String (Int, Int) ),vars_Syn_Procs' :: ([String])}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIprocMapPassDown) =
    (let ( _lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOprocMapCollect,_lhsOvars) = sem _lhsIprocMapPassDown
     in  (Syn_Procs' _lhsOflow _lhsOinterflow _lhsOpretty _lhsOprocMapCollect _lhsOvars))
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
              _lhsOpretty :: ( [String] )
              _hdIfinal :: ([Int])
              _hdIflow :: ([Flow])
              _hdIinit :: Int
              _hdIinterflow :: ([(Int, Int, Int, Int)])
              _hdIpretty :: ( [String] )
              _hdIprocMapCollect :: ( M.Map String (Int, Int) )
              _hdIvars :: ([String])
              _tlIflow :: ([Flow])
              _tlIinterflow :: ([(Int, Int, Int, Int)])
              _tlIpretty :: ( [String] )
              _tlIprocMapCollect :: ( M.Map String (Int, Int) )
              _tlIvars :: ([String])
              _lhsOflow =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _hdIflow ++ _tlIflow
                   {-# LINE 1084 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   _hdIinterflow ++ _tlIinterflow
                   {-# LINE 1089 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 328 "AttributeGrammar.ag" #-}
                   M.union _hdIprocMapCollect _tlIprocMapCollect
                   {-# LINE 1094 "AttributeGrammar.hs" #-}
                   )
              _hdOprocMapPassDown =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1099 "AttributeGrammar.hs" #-}
                   )
              _tlOprocMapPassDown =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1104 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 361 "AttributeGrammar.ag" #-}
                   L.nub (_hdIvars ++ _tlIvars)
                   {-# LINE 1109 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 580 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1114 "AttributeGrammar.hs" #-}
                   )
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIinterflow,_hdIpretty,_hdIprocMapCollect,_hdIvars) =
                  hd_ _hdOprocMapPassDown
              ( _tlIflow,_tlIinterflow,_tlIpretty,_tlIprocMapCollect,_tlIvars) =
                  tl_ _tlOprocMapPassDown
          in  ( _lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOprocMapCollect,_lhsOvars)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
              _lhsOvars :: ([String])
              _lhsOpretty :: ( [String] )
              _lhsOflow =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1132 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 266 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1137 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 326 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1142 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 359 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1147 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 578 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1152 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOprocMapCollect,_lhsOvars)))
-- Program -----------------------------------------------------
data Program = Program (Procs) (Stat)
             deriving ( Show)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Program _procs _stat) =
    (sem_Program_Program (sem_Procs _procs) (sem_Stat _stat))
-- semantic domain
type T_Program = ( Program')
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {labelled_Syn_Program :: Program'}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program) =
    (let ( _lhsOlabelled) = sem
     in  (Syn_Program _lhsOlabelled))
sem_Program_Program :: T_Procs ->
                       T_Stat ->
                       T_Program
sem_Program_Program procs_ stat_ =
    (let _procsOlabel :: Int
         _statOlabel :: Int
         _lhsOlabelled :: Program'
         _procsIlabel :: Int
         _procsIlabelled :: Procs'
         _statIlabel :: Int
         _statIlabelled :: Stat'
         _procsOlabel =
             ({-# LINE 475 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1187 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 476 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1192 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 477 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 1197 "AttributeGrammar.hs" #-}
              )
         ( _procsIlabel,_procsIlabelled) =
             procs_ _procsOlabel
         ( _statIlabel,_statIlabelled) =
             stat_ _statOlabel
     in  ( _lhsOlabelled))
-- Program' ----------------------------------------------------
data Program' = Program' (Procs') (Stat')
              deriving ( Show)
-- cata
sem_Program' :: (Program') ->
                (T_Program')
sem_Program' (Program' _procs _stat) =
    (sem_Program'_Program' (sem_Procs' _procs) (sem_Stat' _stat))
-- semantic domain
type T_Program' = ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),(M.Map Int (S.Set String)),(M.Map Int (S.Set String)),( Int -> (S.Set String -> S.Set String) ),String,( M.Map String (Int, Int) ),([String]))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ([Int]),flow_Syn_Program' :: ([Flow]),init_Syn_Program' :: Int,interflow_Syn_Program' :: ([(Int, Int, Int, Int)]),lvGen_Syn_Program' :: (M.Map Int (S.Set String)),lvKill_Syn_Program' :: (M.Map Int (S.Set String)),lvLambda_Syn_Program' :: ( Int -> (S.Set String -> S.Set String) ),pretty_Syn_Program' :: String,procMapCollect_Syn_Program' :: ( M.Map String (Int, Int) ),vars_Syn_Program' :: ([String])}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlvGen,_lhsOlvKill,_lhsOlvLambda,_lhsOpretty,_lhsOprocMapCollect,_lhsOvars) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOlvGen _lhsOlvKill _lhsOlvLambda _lhsOpretty _lhsOprocMapCollect _lhsOvars))
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
         _lhsOlvLambda :: ( Int -> (S.Set String -> S.Set String) )
         _lhsOlvKill :: (M.Map Int (S.Set String))
         _lhsOlvGen :: (M.Map Int (S.Set String))
         _lhsOpretty :: String
         _procsIflow :: ([Flow])
         _procsIinterflow :: ([(Int, Int, Int, Int)])
         _procsIpretty :: ( [String] )
         _procsIprocMapCollect :: ( M.Map String (Int, Int) )
         _procsIvars :: ([String])
         _statIfinal :: ([Int])
         _statIflow :: ([Flow])
         _statIinit :: Int
         _statIinterflow :: ([(Int, Int, Int, Int)])
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIlvGen :: (M.Map Int (S.Set String))
         _statIlvKill :: (M.Map Int (S.Set String))
         _statIpretty :: ( [String] )
         _statIvars :: ([String])
         _lhsOinit =
             ({-# LINE 106 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1256 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 154 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1261 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _statIflow ++ _procsIflow
              {-# LINE 1266 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 258 "AttributeGrammar.ag" #-}
              _statIinterflow ++ _procsIinterflow
              {-# LINE 1271 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 315 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1276 "AttributeGrammar.hs" #-}
              )
         _statOprocMapPassDown =
             ({-# LINE 316 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1281 "AttributeGrammar.hs" #-}
              )
         _procsOprocMapPassDown =
             ({-# LINE 317 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1286 "AttributeGrammar.hs" #-}
              )
         _lhsOvars =
             ({-# LINE 350 "AttributeGrammar.ag" #-}
              L.nub (_statIvars ++ _procsIvars)
              {-# LINE 1291 "AttributeGrammar.hs" #-}
              )
         _lhsOlvLambda =
             ({-# LINE 418 "AttributeGrammar.ag" #-}
              genLambda _statIlvGen _statIlvKill
              {-# LINE 1296 "AttributeGrammar.hs" #-}
              )
         _lhsOlvKill =
             ({-# LINE 423 "AttributeGrammar.ag" #-}
              _statIlvKill
              {-# LINE 1301 "AttributeGrammar.hs" #-}
              )
         _lhsOlvGen =
             ({-# LINE 439 "AttributeGrammar.ag" #-}
              _statIlvGen
              {-# LINE 1306 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 574 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1311 "AttributeGrammar.hs" #-}
              )
         ( _procsIflow,_procsIinterflow,_procsIpretty,_procsIprocMapCollect,_procsIvars) =
             procs_ _procsOprocMapPassDown
         ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlvGen,_statIlvKill,_statIpretty,_statIvars) =
             stat_ _statOprocMapPassDown
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOlvGen,_lhsOlvKill,_lhsOlvLambda,_lhsOpretty,_lhsOprocMapCollect,_lhsOvars))
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
              ( Int,Stat')
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Int}
data Syn_Stat = Syn_Stat {label_Syn_Stat :: Int,labelled_Syn_Stat :: Stat'}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled) = sem _lhsIlabel
     in  (Syn_Stat _lhsOlabel _lhsOlabelled))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 493 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1378 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 494 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1383 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
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
              _stat1Ilabel :: Int
              _stat1Ilabelled :: Stat'
              _stat2Ilabel :: Int
              _stat2Ilabelled :: Stat'
              _stat1Olabel =
                  ({-# LINE 497 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1403 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 498 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1408 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 499 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1413 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 500 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1418 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ilabel,_stat1Ilabelled) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Ilabelled) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_While :: BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel ->
         (let _statOlabel :: Int
              _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statOlabel =
                  ({-# LINE 503 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1438 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 504 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1443 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 505 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1448 "AttributeGrammar.hs" #-}
                   )
              ( _statIlabel,_statIlabelled) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Call :: String ->
                 Exprs ->
                 String ->
                 T_Stat
sem_Stat_Call name_ params_ out_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 508 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1464 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 509 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1469 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_IAssign :: String ->
                    IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 512 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1482 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 513 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1487 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_BAssign :: String ->
                    BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 516 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1500 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 517 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1505 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _stat1Olabel :: Int
              _stat2Olabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel :: Int
              _stat1Ilabel :: Int
              _stat1Ilabelled :: Stat'
              _stat2Ilabel :: Int
              _stat2Ilabelled :: Stat'
              _stat1Olabel =
                  ({-# LINE 520 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1524 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 521 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1529 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 522 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1534 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 466 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1539 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ilabel,_stat1Ilabelled) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Ilabelled) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Malloc :: String ->
                   IExpr ->
                   T_Stat
sem_Stat_Malloc name_ size_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 525 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1556 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 526 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1561 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 529 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1573 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 530 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1578 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_RefAssign :: IExpr ->
                      IExpr ->
                      T_Stat
sem_Stat_RefAssign ptr_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 533 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1591 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 534 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1596 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 537 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1607 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 538 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1612 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 541 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1623 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 542 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1628 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
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
               ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),Bool,Bool,(M.Map Int (S.Set String)),(M.Map Int (S.Set String)),( [String] ),([String]))
data Inh_Stat' = Inh_Stat' {procMapPassDown_Inh_Stat' :: ( M.Map String (Int, Int) )}
data Syn_Stat' = Syn_Stat' {final_Syn_Stat' :: ([Int]),flow_Syn_Stat' :: ([Flow]),init_Syn_Stat' :: Int,interflow_Syn_Stat' :: ([(Int, Int, Int, Int)]),isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,lvGen_Syn_Stat' :: (M.Map Int (S.Set String)),lvKill_Syn_Stat' :: (M.Map Int (S.Set String)),pretty_Syn_Stat' :: ( [String] ),vars_Syn_Stat' :: ([String])}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIprocMapPassDown) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars) = sem _lhsIprocMapPassDown
     in  (Syn_Stat' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOisSingle _lhsOisSkip _lhsOlvGen _lhsOlvKill _lhsOpretty _lhsOvars))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOinit =
                  ({-# LINE 114 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1700 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 162 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1705 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 216 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1710 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 272 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1715 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 365 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1720 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 590 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 1725 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 591 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1730 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 592 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1735 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1740 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Skip'.lhs.lvGen"
                   {-# LINE 1745 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _condIprecedence :: Int
              _condIpretty :: String
              _stat1Ifinal :: ([Int])
              _stat1Iflow :: ([Flow])
              _stat1Iinit :: Int
              _stat1Iinterflow :: ([(Int, Int, Int, Int)])
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1IlvGen :: (M.Map Int (S.Set String))
              _stat1IlvKill :: (M.Map Int (S.Set String))
              _stat1Ipretty :: ( [String] )
              _stat1Ivars :: ([String])
              _stat2Ifinal :: ([Int])
              _stat2Iflow :: ([Flow])
              _stat2Iinit :: Int
              _stat2Iinterflow :: ([(Int, Int, Int, Int)])
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2IlvGen :: (M.Map Int (S.Set String))
              _stat2IlvKill :: (M.Map Int (S.Set String))
              _stat2Ipretty :: ( [String] )
              _stat2Ivars :: ([String])
              _lhsOinit =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 1792 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 1797 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 219 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _stat1Iinit) : Intra (labelc_, _stat2Iinit) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 1802 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 1807 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1812 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 335 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1817 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 1822 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 429 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 1827 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 445 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvGen _stat2IlvGen
                   {-# LINE 1832 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 594 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 1843 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 601 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1848 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 602 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1853 "AttributeGrammar.hs" #-}
                   )
              ( _condIprecedence,_condIpretty) =
                  cond_
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1IlvGen,_stat1IlvKill,_stat1Ipretty,_stat1Ivars) =
                  stat1_ _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2IlvGen,_stat2IlvKill,_stat2Ipretty,_stat2Ivars) =
                  stat2_ _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _condIprecedence :: Int
              _condIpretty :: String
              _statIfinal :: ([Int])
              _statIflow :: ([Flow])
              _statIinit :: Int
              _statIinterflow :: ([(Int, Int, Int, Int)])
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIlvGen :: (M.Map Int (S.Set String))
              _statIlvKill :: (M.Map Int (S.Set String))
              _statIpretty :: ( [String] )
              _statIvars :: ([String])
              _lhsOinit =
                  ({-# LINE 120 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 1894 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 168 "AttributeGrammar.ag" #-}
                   [labelc_]
                   {-# LINE 1899 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 222 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _statIinit) : map (\x -> Intra (x, labelc_)) _statIfinal ++ _statIflow
                   {-# LINE 1904 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1909 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1914 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   _statIvars
                   {-# LINE 1919 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 431 "AttributeGrammar.ag" #-}
                   _statIlvKill
                   {-# LINE 1924 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 447 "AttributeGrammar.ag" #-}
                   _statIlvGen
                   {-# LINE 1929 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 604 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 1936 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 607 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1941 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 608 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1946 "AttributeGrammar.hs" #-}
                   )
              ( _condIprecedence,_condIpretty) =
                  cond_
              ( _statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIlvGen,_statIlvKill,_statIpretty,_statIvars) =
                  stat_ _statOprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _paramsIpretty :: String
              _lhsOinit =
                  ({-# LINE 123 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 1975 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 171 "AttributeGrammar.ag" #-}
                   [labelExit_]
                   {-# LINE 1980 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 225 "AttributeGrammar.ag" #-}
                   (\(x, y) -> Inter (labelCall_, x) : [Inter (y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 1985 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 281 "AttributeGrammar.ag" #-}
                   (\(x, y) -> [(labelCall_, x, y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 1990 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 375 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1995 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 610 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
                   {-# LINE 2000 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 611 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2005 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 612 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2010 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2015 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Call'.lhs.lvGen"
                   {-# LINE 2020 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIpretty) =
                  params_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _valIfreeVars :: (S.Set String)
              _valIprecedence :: Int
              _valIpretty :: String
              _lhsOinit =
                  ({-# LINE 126 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2047 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2052 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 229 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2057 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2062 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 378 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2067 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 433 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ S.singleton name_
                   {-# LINE 2072 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 449 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ _valIfreeVars
                   {-# LINE 2077 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 614 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2082 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 615 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2087 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 616 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2092 "AttributeGrammar.hs" #-}
                   )
              ( _valIfreeVars,_valIprecedence,_valIpretty) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _valIprecedence :: Int
              _valIpretty :: String
              _lhsOinit =
                  ({-# LINE 129 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2118 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 177 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2123 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 232 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2128 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2133 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 381 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2138 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 618 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2143 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 619 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2148 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 620 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2153 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2158 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.BAssign'.lhs.lvGen"
                   {-# LINE 2163 "AttributeGrammar.hs" #-}
                   )
              ( _valIprecedence,_valIpretty) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _stat1Ifinal :: ([Int])
              _stat1Iflow :: ([Flow])
              _stat1Iinit :: Int
              _stat1Iinterflow :: ([(Int, Int, Int, Int)])
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1IlvGen :: (M.Map Int (S.Set String))
              _stat1IlvKill :: (M.Map Int (S.Set String))
              _stat1Ipretty :: ( [String] )
              _stat1Ivars :: ([String])
              _stat2Ifinal :: ([Int])
              _stat2Iflow :: ([Flow])
              _stat2Iinit :: Int
              _stat2Iinterflow :: ([(Int, Int, Int, Int)])
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2IlvGen :: (M.Map Int (S.Set String))
              _stat2IlvKill :: (M.Map Int (S.Set String))
              _stat2Ipretty :: ( [String] )
              _stat2Ivars :: ([String])
              _lhsOinit =
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2208 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 180 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2213 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 235 "AttributeGrammar.ag" #-}
                   _stat1Iflow ++ _stat2Iflow ++ map (\x -> Intra (x, _stat2Iinit)) _stat1Ifinal
                   {-# LINE 2218 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 2223 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2228 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 342 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2233 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 384 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 2238 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 427 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 2243 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 443 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvGen _stat2IlvGen
                   {-# LINE 2248 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 622 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2253 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 623 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2258 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 624 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2263 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1IlvGen,_stat1IlvKill,_stat1Ipretty,_stat1Ivars) =
                  stat1_ _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2IlvGen,_stat2IlvKill,_stat2Ipretty,_stat2Ivars) =
                  stat2_ _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _sizeIfreeVars :: (S.Set String)
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _lhsOinit =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2292 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 183 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2297 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 238 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2302 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 294 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2307 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 387 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2312 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 626 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2317 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 627 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2322 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 628 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2327 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2332 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Malloc'.lhs.lvGen"
                   {-# LINE 2337 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIfreeVars,_sizeIprecedence,_sizeIpretty) =
                  size_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _ptrIfreeVars :: (S.Set String)
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _lhsOinit =
                  ({-# LINE 138 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2363 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2368 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 241 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2373 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2378 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 390 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2383 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 630 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2388 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 631 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2393 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 632 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2398 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2403 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Free'.lhs.lvGen"
                   {-# LINE 2408 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty) =
                  ptr_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _ptrIfreeVars :: (S.Set String)
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _valIfreeVars :: (S.Set String)
              _valIprecedence :: Int
              _valIpretty :: String
              _lhsOinit =
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2438 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 189 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2443 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 244 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2448 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2453 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 393 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2458 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 634 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2463 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 635 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2468 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 636 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2473 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2478 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.RefAssign'.lhs.lvGen"
                   {-# LINE 2483 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty) =
                  ptr_
              ( _valIfreeVars,_valIprecedence,_valIpretty) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOinit =
                  ({-# LINE 144 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2507 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 192 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2512 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 247 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2517 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2522 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 396 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2527 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 638 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2532 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 639 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2537 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 640 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2542 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2547 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Continue'.lhs.lvGen"
                   {-# LINE 2552 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([Flow])
              _lhsOinterflow :: ([(Int, Int, Int, Int)])
              _lhsOvars :: ([String])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOlvKill :: (M.Map Int (S.Set String))
              _lhsOlvGen :: (M.Map Int (S.Set String))
              _lhsOinit =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2572 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2577 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 250 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2582 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2587 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 399 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2592 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 642 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2597 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 643 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2602 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 644 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2607 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 412 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2612 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Break'.lhs.lvGen"
                   {-# LINE 2617 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))