

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 11 "AttributeGrammar.hs" #-}

{-# LINE 401 "AttributeGrammar.ag" #-}

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
{-# LINE 33 "AttributeGrammar.hs" #-}

{-# LINE 496 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 40 "AttributeGrammar.hs" #-}
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
             ({-# LINE 527 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 97 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 528 "AttributeGrammar.ag" #-}
              10
              {-# LINE 102 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 530 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 113 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 531 "AttributeGrammar.ag" #-}
              10
              {-# LINE 118 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 533 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 134 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 534 "AttributeGrammar.ag" #-}
              4
              {-# LINE 139 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 536 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 159 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 537 "AttributeGrammar.ag" #-}
              4
              {-# LINE 164 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 539 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 184 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 540 "AttributeGrammar.ag" #-}
              4
              {-# LINE 189 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 542 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 209 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 543 "AttributeGrammar.ag" #-}
              4
              {-# LINE 214 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 545 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 234 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 546 "AttributeGrammar.ag" #-}
              4
              {-# LINE 239 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
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
             ({-# LINE 548 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 259 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 549 "AttributeGrammar.ag" #-}
              4
              {-# LINE 264 "AttributeGrammar.hs" #-}
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
             ({-# LINE 551 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 284 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 552 "AttributeGrammar.ag" #-}
              3
              {-# LINE 289 "AttributeGrammar.hs" #-}
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
             ({-# LINE 554 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 309 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 555 "AttributeGrammar.ag" #-}
              2
              {-# LINE 314 "AttributeGrammar.hs" #-}
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
             ({-# LINE 557 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 331 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 558 "AttributeGrammar.ag" #-}
              10
              {-# LINE 336 "AttributeGrammar.hs" #-}
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
    (let _iExprIprecedence :: Int
         _iExprIpretty :: String
         ( _iExprIprecedence,_iExprIpretty) =
             iExpr_
     in  ( ))
sem_Code_CStat :: (T_Stat') ->
                  T_Code
sem_Code_CStat stat'_ =
    (let _stat'OprocMapPassDown :: ( M.Map String (Int, Int) )
         _stat'Ifinal :: ([Int])
         _stat'Iflow :: ([(Int, Int)])
         _stat'Iinit :: Int
         _stat'IisSingle :: Bool
         _stat'IisSkip :: Bool
         _stat'Ipretty :: ( [String] )
         _stat'OprocMapPassDown =
             ({-# LINE 259 "AttributeGrammar.ag" #-}
              error "missing rule: Code.CStat.stat'.procMapPassDown"
              {-# LINE 399 "AttributeGrammar.hs" #-}
              )
         ( _stat'Ifinal,_stat'Iflow,_stat'Iinit,_stat'IisSingle,_stat'IisSkip,_stat'Ipretty) =
             stat'_ _stat'OprocMapPassDown
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let _proc'Ifinal :: ([Int])
         _proc'Iflow :: ([(Int, Int)])
         _proc'Iinit :: Int
         _proc'Ipretty :: ( [String] )
         _proc'IprocMapCollect :: ( M.Map String (Int, Int) )
         ( _proc'Ifinal,_proc'Iflow,_proc'Iinit,_proc'Ipretty,_proc'IprocMapCollect) =
             proc'_
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let _program'Ifinal :: ([Int])
         _program'Iflow :: ([(Int, Int)])
         _program'Iinit :: Int
         _program'Ipretty :: String
         _program'IprocMapCollect :: ( M.Map String (Int, Int) )
         ( _program'Ifinal,_program'Iflow,_program'Iinit,_program'Ipretty,_program'IprocMapCollect) =
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
             ({-# LINE 562 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 456 "AttributeGrammar.hs" #-}
              )
         ( _exprIprecedence,_exprIpretty) =
             expr_
     in  ( _lhsOpretty))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOpretty :: String
         _exprIprecedence :: Int
         _exprIpretty :: String
         _lhsOpretty =
             ({-# LINE 564 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 470 "AttributeGrammar.hs" #-}
              )
         ( _exprIprecedence,_exprIpretty) =
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
             ({-# LINE 570 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 502 "AttributeGrammar.hs" #-}
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
             ({-# LINE 568 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 515 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOpretty))
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
type T_IExpr = ( Int,String)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOprecedence,_lhsOpretty) = sem
     in  (Syn_IExpr _lhsOprecedence _lhsOpretty))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 504 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 562 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 505 "AttributeGrammar.ag" #-}
              10
              {-# LINE 567 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 507 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 578 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 508 "AttributeGrammar.ag" #-}
              10
              {-# LINE 583 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 510 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 599 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 511 "AttributeGrammar.ag" #-}
              6
              {-# LINE 604 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 513 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 624 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 514 "AttributeGrammar.ag" #-}
              6
              {-# LINE 629 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 516 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 649 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 517 "AttributeGrammar.ag" #-}
              7
              {-# LINE 654 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 519 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 674 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 520 "AttributeGrammar.ag" #-}
              7
              {-# LINE 679 "AttributeGrammar.hs" #-}
              )
         ( _leftIprecedence,_leftIpretty) =
             left_
         ( _rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _lhsOpretty =
             ({-# LINE 522 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 696 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 523 "AttributeGrammar.ag" #-}
              10
              {-# LINE 701 "AttributeGrammar.hs" #-}
              )
         ( _ptrIprecedence,_ptrIpretty) =
             ptr_
     in  ( _lhsOprecedence,_lhsOpretty))
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
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 740 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 745 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 750 "AttributeGrammar.hs" #-}
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
type T_Proc' = ( ([Int]),([(Int, Int)]),Int,( [String] ),( M.Map String (Int, Int) ))
data Inh_Proc' = Inh_Proc' {}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ([Int]),flow_Syn_Proc' :: ([(Int, Int)]),init_Syn_Proc' :: Int,pretty_Syn_Proc' :: ( [String] ),procMapCollect_Syn_Proc' :: ( M.Map String (Int, Int) )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOprocMapCollect) = sem
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOprocMapCollect))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelReturn_ name_ inp_ out_ stat_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOflow :: ([(Int, Int)])
         _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
         _lhsOpretty :: ( [String] )
         _statOprocMapPassDown :: ( M.Map String (Int, Int) )
         _statIfinal :: ([Int])
         _statIflow :: ([(Int, Int)])
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 106 "AttributeGrammar.ag" #-}
              labelEntry_
              {-# LINE 796 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 160 "AttributeGrammar.ag" #-}
              [labelReturn_]
              {-# LINE 801 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _statIflow
              {-# LINE 806 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 268 "AttributeGrammar.ag" #-}
              M.singleton name_ (labelEntry_, labelReturn_)
              {-# LINE 811 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 434 "AttributeGrammar.ag" #-}
              ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
               ++ indent _statIpretty
               ++ ["end" ++ showLabel labelReturn_ ++ ";"]
              {-# LINE 818 "AttributeGrammar.hs" #-}
              )
         _statOprocMapPassDown =
             ({-# LINE 259 "AttributeGrammar.ag" #-}
              error "missing rule: Proc'.Proc'.stat.procMapPassDown"
              {-# LINE 823 "AttributeGrammar.hs" #-}
              )
         ( _statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_ _statOprocMapPassDown
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOprocMapCollect))
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
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 862 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 867 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 872 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 877 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 892 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 897 "AttributeGrammar.hs" #-}
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
type T_Procs' = ( ([(Int, Int)]),( [String] ),( M.Map String (Int, Int) ))
data Inh_Procs' = Inh_Procs' {}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ([(Int, Int)]),pretty_Syn_Procs' :: ( [String] ),procMapCollect_Syn_Procs' :: ( M.Map String (Int, Int) )}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs') =
    (let ( _lhsOflow,_lhsOpretty,_lhsOprocMapCollect) = sem
     in  (Syn_Procs' _lhsOflow _lhsOpretty _lhsOprocMapCollect))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (let _lhsOflow :: ([(Int, Int)])
         _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
         _lhsOpretty :: ( [String] )
         _hdIfinal :: ([Int])
         _hdIflow :: ([(Int, Int)])
         _hdIinit :: Int
         _hdIpretty :: ( [String] )
         _hdIprocMapCollect :: ( M.Map String (Int, Int) )
         _tlIflow :: ([(Int, Int)])
         _tlIpretty :: ( [String] )
         _tlIprocMapCollect :: ( M.Map String (Int, Int) )
         _lhsOflow =
             ({-# LINE 214 "AttributeGrammar.ag" #-}
              _hdIflow ++ _tlIflow
              {-# LINE 935 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 274 "AttributeGrammar.ag" #-}
              M.union _hdIprocMapCollect _tlIprocMapCollect
              {-# LINE 940 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 430 "AttributeGrammar.ag" #-}
              _hdIpretty ++ _tlIpretty
              {-# LINE 945 "AttributeGrammar.hs" #-}
              )
         ( _hdIfinal,_hdIflow,_hdIinit,_hdIpretty,_hdIprocMapCollect) =
             hd_
         ( _tlIflow,_tlIpretty,_tlIprocMapCollect) =
             tl_
     in  ( _lhsOflow,_lhsOpretty,_lhsOprocMapCollect))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (let _lhsOflow :: ([(Int, Int)])
         _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
         _lhsOpretty :: ( [String] )
         _lhsOflow =
             ({-# LINE 212 "AttributeGrammar.ag" #-}
              []
              {-# LINE 960 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 272 "AttributeGrammar.ag" #-}
              M.empty
              {-# LINE 965 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 428 "AttributeGrammar.ag" #-}
              []
              {-# LINE 970 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOflow,_lhsOpretty,_lhsOprocMapCollect))
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
             ({-# LINE 325 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1005 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 326 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1010 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 327 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 1015 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ([Int]),([(Int, Int)]),Int,String,( M.Map String (Int, Int) ))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ([Int]),flow_Syn_Program' :: ([(Int, Int)]),init_Syn_Program' :: Int,pretty_Syn_Program' :: String,procMapCollect_Syn_Program' :: ( M.Map String (Int, Int) )}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOprocMapCollect) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOprocMapCollect))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOflow :: ([(Int, Int)])
         _lhsOprocMapCollect :: ( M.Map String (Int, Int) )
         _statOprocMapPassDown :: ( M.Map String (Int, Int) )
         _lhsOpretty :: String
         _procsIflow :: ([(Int, Int)])
         _procsIpretty :: ( [String] )
         _procsIprocMapCollect :: ( M.Map String (Int, Int) )
         _statIfinal :: ([Int])
         _statIflow :: ([(Int, Int)])
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 102 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1062 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 156 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1067 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 204 "AttributeGrammar.ag" #-}
              _statIflow ++ _procsIflow
              {-# LINE 1072 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 263 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1077 "AttributeGrammar.hs" #-}
              )
         _statOprocMapPassDown =
             ({-# LINE 264 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1082 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 424 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1087 "AttributeGrammar.hs" #-}
              )
         ( _procsIflow,_procsIpretty,_procsIprocMapCollect) =
             procs_
         ( _statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_ _statOprocMapPassDown
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOprocMapCollect))
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
                  ({-# LINE 343 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1154 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1159 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 347 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1179 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 348 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1184 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 349 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1189 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 350 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1194 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1214 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 354 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1219 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1224 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 358 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1240 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 359 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1245 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1258 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 363 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1263 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 366 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1276 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 367 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1281 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 370 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1300 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1305 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 372 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1310 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1315 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 375 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1332 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 376 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1337 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 379 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1349 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 380 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1354 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 383 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1367 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 384 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1372 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 387 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1383 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 388 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1388 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 391 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1399 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 392 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1404 "AttributeGrammar.hs" #-}
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
               ( ([Int]),([(Int, Int)]),Int,Bool,Bool,( [String] ))
data Inh_Stat' = Inh_Stat' {procMapPassDown_Inh_Stat' :: ( M.Map String (Int, Int) )}
data Syn_Stat' = Syn_Stat' {final_Syn_Stat' :: ([Int]),flow_Syn_Stat' :: ([(Int, Int)]),init_Syn_Stat' :: Int,isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIprocMapPassDown) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty) = sem _lhsIprocMapPassDown
     in  (Syn_Stat' _lhsOfinal _lhsOflow _lhsOinit _lhsOisSingle _lhsOisSkip _lhsOpretty))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit =
                  ({-# LINE 116 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1472 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 164 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1477 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 218 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1482 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 440 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 1487 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 441 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1492 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 442 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1497 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _stat1OprocMapPassDown :: ( M.Map String (Int, Int) )
              _stat2OprocMapPassDown :: ( M.Map String (Int, Int) )
              _condIprecedence :: Int
              _condIpretty :: String
              _stat1Ifinal :: ([Int])
              _stat1Iflow :: ([(Int, Int)])
              _stat1Iinit :: Int
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat2Ifinal :: ([Int])
              _stat2Iflow :: ([(Int, Int)])
              _stat2Iinit :: Int
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _lhsOinit =
                  ({-# LINE 119 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 1532 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 167 "AttributeGrammar.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 1537 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 221 "AttributeGrammar.ag" #-}
                   (labelc_, _stat1Iinit) : (labelc_, _stat2Iinit) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 1542 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 444 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 1553 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 451 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1558 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 452 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1563 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1568 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1573 "AttributeGrammar.hs" #-}
                   )
              ( _condIprecedence,_condIpretty) =
                  cond_
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
                  stat1_ _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
                  stat2_ _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _statOprocMapPassDown :: ( M.Map String (Int, Int) )
              _condIprecedence :: Int
              _condIpretty :: String
              _statIfinal :: ([Int])
              _statIflow :: ([(Int, Int)])
              _statIinit :: Int
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _lhsOinit =
                  ({-# LINE 122 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 1606 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 170 "AttributeGrammar.ag" #-}
                   [labelc_]
                   {-# LINE 1611 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 224 "AttributeGrammar.ag" #-}
                   (labelc_, _statIinit) : map (\x -> (x, labelc_)) _statIfinal ++ _statIflow
                   {-# LINE 1616 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 454 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 1623 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 457 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1628 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 458 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1633 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1638 "AttributeGrammar.hs" #-}
                   )
              ( _condIprecedence,_condIpretty) =
                  cond_
              ( _statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
                  stat_ _statOprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
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
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOflow :: ([(Int, Int)])
              _paramsIpretty :: String
              _lhsOinit =
                  ({-# LINE 125 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 1663 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 173 "AttributeGrammar.ag" #-}
                   [labelCall_]
                   {-# LINE 1668 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 460 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
                   {-# LINE 1673 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 461 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1678 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 462 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1683 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Call'.lhs.flow"
                   {-# LINE 1688 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIpretty) =
                  params_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _valIprecedence :: Int
              _valIpretty :: String
              _lhsOinit =
                  ({-# LINE 128 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1710 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 176 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1715 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 231 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1720 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 464 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 1725 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 465 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1730 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 466 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1735 "AttributeGrammar.hs" #-}
                   )
              ( _valIprecedence,_valIpretty) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _valIprecedence :: Int
              _valIpretty :: String
              _lhsOinit =
                  ({-# LINE 131 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1757 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 179 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1762 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 234 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1767 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 468 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 1772 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 469 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1777 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 470 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1782 "AttributeGrammar.hs" #-}
                   )
              ( _valIprecedence,_valIpretty) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _stat1OprocMapPassDown :: ( M.Map String (Int, Int) )
              _stat2OprocMapPassDown :: ( M.Map String (Int, Int) )
              _stat1Ifinal :: ([Int])
              _stat1Iflow :: ([(Int, Int)])
              _stat1Iinit :: Int
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat2Ifinal :: ([Int])
              _stat2Iflow :: ([(Int, Int)])
              _stat2Iinit :: Int
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _lhsOinit =
                  ({-# LINE 134 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 1815 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 182 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 1820 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 237 "AttributeGrammar.ag" #-}
                   _stat1Iflow ++ _stat2Iflow ++ map (\x -> (x, _stat2Iinit)) _stat1Ifinal
                   {-# LINE 1825 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 472 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 1830 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 473 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1835 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 474 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1840 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1845 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1850 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
                  stat1_ _stat1OprocMapPassDown
              ( _stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
                  stat2_ _stat2OprocMapPassDown
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _lhsOinit =
                  ({-# LINE 137 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1874 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 185 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1879 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 240 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1884 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 476 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 1889 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 477 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1894 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 478 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1899 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIprecedence,_sizeIpretty) =
                  size_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _lhsOinit =
                  ({-# LINE 140 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1920 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 188 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1925 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 243 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1930 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 480 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 1935 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 481 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1940 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 482 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1945 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIprecedence,_ptrIpretty) =
                  ptr_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _valIprecedence :: Int
              _valIpretty :: String
              _lhsOinit =
                  ({-# LINE 143 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1969 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 191 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1974 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 246 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1979 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 484 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 1984 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 485 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1989 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 486 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1994 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIprecedence,_ptrIpretty) =
                  ptr_
              ( _valIprecedence,_valIpretty) =
                  val_
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit =
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2014 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2019 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 249 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2024 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 488 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2029 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 489 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2034 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 490 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2039 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIprocMapPassDown ->
         (let _lhsOinit :: Int
              _lhsOfinal :: ([Int])
              _lhsOflow :: ([(Int, Int)])
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit =
                  ({-# LINE 149 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2055 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 197 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2060 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 252 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2065 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 492 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2070 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 493 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2075 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 494 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2080 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty)))