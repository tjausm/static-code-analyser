

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 11 "AttributeGrammar.hs" #-}

{-# LINE 290 "AttributeGrammar.ag" #-}

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

{-# LINE 385 "AttributeGrammar.ag" #-}

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
             ({-# LINE 416 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 97 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
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
             ({-# LINE 419 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 113 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
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
             ({-# LINE 422 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 134 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 423 "AttributeGrammar.ag" #-}
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
             ({-# LINE 425 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 159 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 426 "AttributeGrammar.ag" #-}
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
             ({-# LINE 428 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 184 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 429 "AttributeGrammar.ag" #-}
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
             ({-# LINE 431 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 209 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 432 "AttributeGrammar.ag" #-}
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
             ({-# LINE 434 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 234 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 435 "AttributeGrammar.ag" #-}
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
             ({-# LINE 437 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 259 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 438 "AttributeGrammar.ag" #-}
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
             ({-# LINE 440 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 284 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 441 "AttributeGrammar.ag" #-}
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
             ({-# LINE 443 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 309 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 444 "AttributeGrammar.ag" #-}
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
             ({-# LINE 446 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 331 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 447 "AttributeGrammar.ag" #-}
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
    (let _stat'Ifinal :: ([Int])
         _stat'Iinit :: Int
         _stat'IisSingle :: Bool
         _stat'IisSkip :: Bool
         _stat'Ipretty :: ( [String] )
         ( _stat'Ifinal,_stat'Iinit,_stat'IisSingle,_stat'IisSkip,_stat'Ipretty) =
             stat'_
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let _proc'Ifinal :: ([Int])
         _proc'Iinit :: Int
         _proc'Ipretty :: ( [String] )
         ( _proc'Ifinal,_proc'Iinit,_proc'Ipretty) =
             proc'_
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let _program'Ifinal :: ([Int])
         _program'Iinit :: Int
         _program'Ipretty :: String
         ( _program'Ifinal,_program'Iinit,_program'Ipretty) =
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
             ({-# LINE 451 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 445 "AttributeGrammar.hs" #-}
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
             ({-# LINE 453 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 459 "AttributeGrammar.hs" #-}
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
             ({-# LINE 459 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 491 "AttributeGrammar.hs" #-}
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
             ({-# LINE 457 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 504 "AttributeGrammar.hs" #-}
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
             ({-# LINE 393 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 551 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 394 "AttributeGrammar.ag" #-}
              10
              {-# LINE 556 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 396 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 567 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 397 "AttributeGrammar.ag" #-}
              10
              {-# LINE 572 "AttributeGrammar.hs" #-}
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
             ({-# LINE 399 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 588 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 400 "AttributeGrammar.ag" #-}
              6
              {-# LINE 593 "AttributeGrammar.hs" #-}
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
             ({-# LINE 402 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 613 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 403 "AttributeGrammar.ag" #-}
              6
              {-# LINE 618 "AttributeGrammar.hs" #-}
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
             ({-# LINE 405 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 638 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 406 "AttributeGrammar.ag" #-}
              7
              {-# LINE 643 "AttributeGrammar.hs" #-}
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
             ({-# LINE 408 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 663 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 409 "AttributeGrammar.ag" #-}
              7
              {-# LINE 668 "AttributeGrammar.hs" #-}
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
             ({-# LINE 411 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 685 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 412 "AttributeGrammar.ag" #-}
              10
              {-# LINE 690 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 226 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 729 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 227 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 734 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 228 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 739 "AttributeGrammar.hs" #-}
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
type T_Proc' = ( ([Int]),Int,( [String] ))
data Inh_Proc' = Inh_Proc' {}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ([Int]),init_Syn_Proc' :: Int,pretty_Syn_Proc' :: ( [String] )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc') =
    (let ( _lhsOfinal,_lhsOinit,_lhsOpretty) = sem
     in  (Syn_Proc' _lhsOfinal _lhsOinit _lhsOpretty))
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
         _lhsOpretty :: ( [String] )
         _statIfinal :: ([Int])
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 106 "AttributeGrammar.ag" #-}
              labelEntry_
              {-# LINE 781 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 160 "AttributeGrammar.ag" #-}
              [labelReturn_]
              {-# LINE 786 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 323 "AttributeGrammar.ag" #-}
              ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
               ++ indent _statIpretty
               ++ ["end" ++ showLabel labelReturn_ ++ ";"]
              {-# LINE 793 "AttributeGrammar.hs" #-}
              )
         ( _statIfinal,_statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOfinal,_lhsOinit,_lhsOpretty))
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
                  ({-# LINE 222 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 832 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 837 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 842 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 847 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 220 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 862 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 867 "AttributeGrammar.hs" #-}
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
type T_Procs' = ( ( [String] ))
data Inh_Procs' = Inh_Procs' {}
data Syn_Procs' = Syn_Procs' {pretty_Syn_Procs' :: ( [String] )}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs') =
    (let ( _lhsOpretty) = sem
     in  (Syn_Procs' _lhsOpretty))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (let _lhsOpretty :: ( [String] )
         _hdIfinal :: ([Int])
         _hdIinit :: Int
         _hdIpretty :: ( [String] )
         _tlIpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 319 "AttributeGrammar.ag" #-}
              _hdIpretty ++ _tlIpretty
              {-# LINE 899 "AttributeGrammar.hs" #-}
              )
         ( _hdIfinal,_hdIinit,_hdIpretty) =
             hd_
         ( _tlIpretty) =
             tl_
     in  ( _lhsOpretty))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (let _lhsOpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 317 "AttributeGrammar.ag" #-}
              []
              {-# LINE 912 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOpretty))
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
             ({-# LINE 214 "AttributeGrammar.ag" #-}
              1
              {-# LINE 947 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 215 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 952 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 216 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 957 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ([Int]),Int,String)
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ([Int]),init_Syn_Program' :: Int,pretty_Syn_Program' :: String}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOinit,_lhsOpretty) = sem
     in  (Syn_Program' _lhsOfinal _lhsOinit _lhsOpretty))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: String
         _procsIpretty :: ( [String] )
         _statIfinal :: ([Int])
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 102 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 998 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 156 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1003 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 313 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1008 "AttributeGrammar.hs" #-}
              )
         ( _procsIpretty) =
             procs_
         ( _statIfinal,_statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOfinal,_lhsOinit,_lhsOpretty))
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
                  ({-# LINE 232 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1075 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 233 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1080 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 236 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1100 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 237 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1105 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 238 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1110 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 239 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1115 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 242 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1135 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 243 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1140 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 244 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1145 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 247 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1161 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 248 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1166 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 251 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1179 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 252 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1184 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 255 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1197 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 256 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1202 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1221 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 260 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1226 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 261 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1231 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1236 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 264 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1253 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 265 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1258 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1270 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1275 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 272 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1288 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 273 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1293 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 276 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1304 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 277 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1309 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 280 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1320 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 281 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1325 "AttributeGrammar.hs" #-}
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
type T_Stat' = ( ([Int]),Int,Bool,Bool,( [String] ))
data Inh_Stat' = Inh_Stat' {}
data Syn_Stat' = Syn_Stat' {final_Syn_Stat' :: ([Int]),init_Syn_Stat' :: Int,isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat') =
    (let ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty) = sem
     in  (Syn_Stat' _lhsOfinal _lhsOinit _lhsOisSingle _lhsOisSkip _lhsOpretty))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOinit =
             ({-# LINE 116 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1390 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 164 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1395 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 329 "AttributeGrammar.ag" #-}
              ["skip" ++ showLabel label_]
              {-# LINE 1400 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 330 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1405 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 331 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1410 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _condIprecedence :: Int
         _condIpretty :: String
         _stat1Ifinal :: ([Int])
         _stat1Iinit :: Int
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat2Ifinal :: ([Int])
         _stat2Iinit :: Int
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 119 "AttributeGrammar.ag" #-}
              labelc_
              {-# LINE 1439 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 167 "AttributeGrammar.ag" #-}
              _stat1Ifinal ++ _stat2Ifinal
              {-# LINE 1444 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 333 "AttributeGrammar.ag" #-}
              ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
               ++ indent _stat1Ipretty
               ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                       [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                   ++ indent _stat2Ipretty
                   ++ (if _stat2IisSingle then [] else ["}"])
                 )
              {-# LINE 1455 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 340 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1460 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 341 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1465 "AttributeGrammar.hs" #-}
              )
         ( _condIprecedence,_condIpretty) =
             cond_
         ( _stat1Ifinal,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
             stat1_
         ( _stat2Ifinal,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
             stat2_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _condIprecedence :: Int
         _condIpretty :: String
         _statIfinal :: ([Int])
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 122 "AttributeGrammar.ag" #-}
              labelc_
              {-# LINE 1494 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 170 "AttributeGrammar.ag" #-}
              [labelc_]
              {-# LINE 1499 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 343 "AttributeGrammar.ag" #-}
              ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
               ++ indent _statIpretty
               ++ (if _statIisSingle then [] else ["}"])
              {-# LINE 1506 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 346 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1511 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 347 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1516 "AttributeGrammar.hs" #-}
              )
         ( _condIprecedence,_condIpretty) =
             cond_
         ( _statIfinal,_statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _paramsIpretty :: String
         _lhsOinit =
             ({-# LINE 125 "AttributeGrammar.ag" #-}
              labelCall_
              {-# LINE 1539 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 173 "AttributeGrammar.ag" #-}
              [labelCall_]
              {-# LINE 1544 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 349 "AttributeGrammar.ag" #-}
              ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
              {-# LINE 1549 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 350 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1554 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 351 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1559 "AttributeGrammar.hs" #-}
              )
         ( _paramsIpretty) =
             params_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOinit =
             ({-# LINE 128 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1579 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 176 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1584 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 353 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1589 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 354 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1594 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 355 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1599 "AttributeGrammar.hs" #-}
              )
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOinit =
             ({-# LINE 131 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1619 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 179 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1624 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 357 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1629 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 358 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1634 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 359 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1639 "AttributeGrammar.hs" #-}
              )
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _stat1Ifinal :: ([Int])
         _stat1Iinit :: Int
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat2Ifinal :: ([Int])
         _stat2Iinit :: Int
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 134 "AttributeGrammar.ag" #-}
              _stat1Iinit
              {-# LINE 1666 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 182 "AttributeGrammar.ag" #-}
              _stat2Ifinal
              {-# LINE 1671 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 361 "AttributeGrammar.ag" #-}
              addSemicolon _stat1Ipretty ++ _stat2Ipretty
              {-# LINE 1676 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 362 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1681 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 363 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1686 "AttributeGrammar.hs" #-}
              )
         ( _stat1Ifinal,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
             stat1_
         ( _stat2Ifinal,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
             stat2_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _sizeIprecedence :: Int
         _sizeIpretty :: String
         _lhsOinit =
             ({-# LINE 137 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1708 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 185 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1713 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 365 "AttributeGrammar.ag" #-}
              ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1718 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 366 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1723 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 367 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1728 "AttributeGrammar.hs" #-}
              )
         ( _sizeIprecedence,_sizeIpretty) =
             size_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _lhsOinit =
             ({-# LINE 140 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1747 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 188 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1752 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 369 "AttributeGrammar.ag" #-}
              ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1757 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 370 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1762 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 371 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1767 "AttributeGrammar.hs" #-}
              )
         ( _ptrIprecedence,_ptrIpretty) =
             ptr_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
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
              {-# LINE 1789 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 191 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1794 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 373 "AttributeGrammar.ag" #-}
              ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1799 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 374 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1804 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 375 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1809 "AttributeGrammar.hs" #-}
              )
         ( _ptrIprecedence,_ptrIpretty) =
             ptr_
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOinit =
             ({-# LINE 146 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1827 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 194 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1832 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 377 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1837 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 378 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1842 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 379 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1847 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (let _lhsOinit :: Int
         _lhsOfinal :: ([Int])
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOinit =
             ({-# LINE 149 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1861 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 197 "AttributeGrammar.ag" #-}
              [label_]
              {-# LINE 1866 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 381 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1871 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 382 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1876 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 383 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1881 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfinal,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))