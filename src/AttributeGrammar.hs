

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 11 "AttributeGrammar.hs" #-}

{-# LINE 244 "AttributeGrammar.ag" #-}

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

{-# LINE 339 "AttributeGrammar.ag" #-}

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
             ({-# LINE 370 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 97 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 371 "AttributeGrammar.ag" #-}
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
             ({-# LINE 373 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 113 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 374 "AttributeGrammar.ag" #-}
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
             ({-# LINE 376 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 134 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 377 "AttributeGrammar.ag" #-}
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
             ({-# LINE 379 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 159 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 380 "AttributeGrammar.ag" #-}
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
             ({-# LINE 382 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 184 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 383 "AttributeGrammar.ag" #-}
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
             ({-# LINE 385 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 209 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 386 "AttributeGrammar.ag" #-}
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
             ({-# LINE 388 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 234 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 389 "AttributeGrammar.ag" #-}
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
             ({-# LINE 391 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 259 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 392 "AttributeGrammar.ag" #-}
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
             ({-# LINE 394 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 284 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 395 "AttributeGrammar.ag" #-}
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
             ({-# LINE 397 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 309 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 398 "AttributeGrammar.ag" #-}
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
             ({-# LINE 400 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 331 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 401 "AttributeGrammar.ag" #-}
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
    (let _stat'Iinit :: Int
         _stat'IisSingle :: Bool
         _stat'IisSkip :: Bool
         _stat'Ipretty :: ( [String] )
         ( _stat'Iinit,_stat'IisSingle,_stat'IisSkip,_stat'Ipretty) =
             stat'_
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let _proc'Iinit :: Int
         _proc'Ipretty :: ( [String] )
         ( _proc'Iinit,_proc'Ipretty) =
             proc'_
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let _program'Iinit :: Int
         _program'Ipretty :: String
         ( _program'Iinit,_program'Ipretty) =
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
             ({-# LINE 405 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 442 "AttributeGrammar.hs" #-}
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
             ({-# LINE 407 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 456 "AttributeGrammar.hs" #-}
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
             ({-# LINE 413 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 488 "AttributeGrammar.hs" #-}
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
             ({-# LINE 411 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 501 "AttributeGrammar.hs" #-}
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
             ({-# LINE 347 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 548 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 348 "AttributeGrammar.ag" #-}
              10
              {-# LINE 553 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 350 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 564 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 351 "AttributeGrammar.ag" #-}
              10
              {-# LINE 569 "AttributeGrammar.hs" #-}
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
             ({-# LINE 353 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 585 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 354 "AttributeGrammar.ag" #-}
              6
              {-# LINE 590 "AttributeGrammar.hs" #-}
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
             ({-# LINE 356 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 610 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 357 "AttributeGrammar.ag" #-}
              6
              {-# LINE 615 "AttributeGrammar.hs" #-}
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
             ({-# LINE 359 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 635 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 360 "AttributeGrammar.ag" #-}
              7
              {-# LINE 640 "AttributeGrammar.hs" #-}
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
             ({-# LINE 362 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 660 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 363 "AttributeGrammar.ag" #-}
              7
              {-# LINE 665 "AttributeGrammar.hs" #-}
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
             ({-# LINE 365 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 682 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 366 "AttributeGrammar.ag" #-}
              10
              {-# LINE 687 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 180 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 726 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 181 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 731 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 182 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 736 "AttributeGrammar.hs" #-}
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
type T_Proc' = ( Int,( [String] ))
data Inh_Proc' = Inh_Proc' {}
data Syn_Proc' = Syn_Proc' {init_Syn_Proc' :: Int,pretty_Syn_Proc' :: ( [String] )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc') =
    (let ( _lhsOinit,_lhsOpretty) = sem
     in  (Syn_Proc' _lhsOinit _lhsOpretty))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelReturn_ name_ inp_ out_ stat_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 106 "AttributeGrammar.ag" #-}
              labelEntry_
              {-# LINE 776 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 277 "AttributeGrammar.ag" #-}
              ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
               ++ indent _statIpretty
               ++ ["end" ++ showLabel labelReturn_ ++ ";"]
              {-# LINE 783 "AttributeGrammar.hs" #-}
              )
         ( _statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOinit,_lhsOpretty))
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
                  ({-# LINE 176 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 822 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 827 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 832 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 837 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 852 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 857 "AttributeGrammar.hs" #-}
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
         _hdIinit :: Int
         _hdIpretty :: ( [String] )
         _tlIpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 273 "AttributeGrammar.ag" #-}
              _hdIpretty ++ _tlIpretty
              {-# LINE 888 "AttributeGrammar.hs" #-}
              )
         ( _hdIinit,_hdIpretty) =
             hd_
         ( _tlIpretty) =
             tl_
     in  ( _lhsOpretty))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (let _lhsOpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 271 "AttributeGrammar.ag" #-}
              []
              {-# LINE 901 "AttributeGrammar.hs" #-}
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
             ({-# LINE 168 "AttributeGrammar.ag" #-}
              1
              {-# LINE 936 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 169 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 941 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 170 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 946 "AttributeGrammar.hs" #-}
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
type T_Program' = ( Int,String)
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {init_Syn_Program' :: Int,pretty_Syn_Program' :: String}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOinit,_lhsOpretty) = sem
     in  (Syn_Program' _lhsOinit _lhsOpretty))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: String
         _procsIpretty :: ( [String] )
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 102 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 985 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 267 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 990 "AttributeGrammar.hs" #-}
              )
         ( _procsIpretty) =
             procs_
         ( _statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOinit,_lhsOpretty))
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
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1057 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 187 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1062 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 190 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1082 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 191 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1087 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 192 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1092 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 193 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1097 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 196 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1117 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 197 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1122 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1127 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 201 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1143 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 202 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1148 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1161 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1166 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 209 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1179 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1184 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 213 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1203 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 214 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1208 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 215 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1213 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1218 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 218 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1235 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 219 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1240 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 222 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1252 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 223 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1257 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 226 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1270 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 227 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1275 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 230 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1286 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 231 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1291 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 234 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1302 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 235 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1307 "AttributeGrammar.hs" #-}
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
type T_Stat' = ( Int,Bool,Bool,( [String] ))
data Inh_Stat' = Inh_Stat' {}
data Syn_Stat' = Syn_Stat' {init_Syn_Stat' :: Int,isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat') =
    (let ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty) = sem
     in  (Syn_Stat' _lhsOinit _lhsOisSingle _lhsOisSkip _lhsOpretty))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOinit =
             ({-# LINE 116 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1371 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 283 "AttributeGrammar.ag" #-}
              ["skip" ++ showLabel label_]
              {-# LINE 1376 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 284 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1381 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 285 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1386 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _condIprecedence :: Int
         _condIpretty :: String
         _stat1Iinit :: Int
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat2Iinit :: Int
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 119 "AttributeGrammar.ag" #-}
              labelc_
              {-# LINE 1412 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 287 "AttributeGrammar.ag" #-}
              ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
               ++ indent _stat1Ipretty
               ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                       [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                   ++ indent _stat2Ipretty
                   ++ (if _stat2IisSingle then [] else ["}"])
                 )
              {-# LINE 1423 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 294 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1428 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 295 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1433 "AttributeGrammar.hs" #-}
              )
         ( _condIprecedence,_condIpretty) =
             cond_
         ( _stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
             stat1_
         ( _stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
             stat2_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _condIprecedence :: Int
         _condIpretty :: String
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 122 "AttributeGrammar.ag" #-}
              labelc_
              {-# LINE 1460 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 297 "AttributeGrammar.ag" #-}
              ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
               ++ indent _statIpretty
               ++ (if _statIisSingle then [] else ["}"])
              {-# LINE 1467 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 300 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1472 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 301 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1477 "AttributeGrammar.hs" #-}
              )
         ( _condIprecedence,_condIpretty) =
             cond_
         ( _statIinit,_statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _paramsIpretty :: String
         _lhsOinit =
             ({-# LINE 125 "AttributeGrammar.ag" #-}
              labelCall_
              {-# LINE 1499 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 303 "AttributeGrammar.ag" #-}
              ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
              {-# LINE 1504 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 304 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1509 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 305 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1514 "AttributeGrammar.hs" #-}
              )
         ( _paramsIpretty) =
             params_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOinit =
             ({-# LINE 128 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1533 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 307 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1538 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 308 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1543 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 309 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1548 "AttributeGrammar.hs" #-}
              )
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOinit =
             ({-# LINE 131 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1567 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 311 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1572 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 312 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1577 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 313 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1582 "AttributeGrammar.hs" #-}
              )
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _stat1Iinit :: Int
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat2Iinit :: Int
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _lhsOinit =
             ({-# LINE 134 "AttributeGrammar.ag" #-}
              _stat1Iinit
              {-# LINE 1606 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 315 "AttributeGrammar.ag" #-}
              addSemicolon _stat1Ipretty ++ _stat2Ipretty
              {-# LINE 1611 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 316 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1616 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 317 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1621 "AttributeGrammar.hs" #-}
              )
         ( _stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
             stat1_
         ( _stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
             stat2_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _sizeIprecedence :: Int
         _sizeIpretty :: String
         _lhsOinit =
             ({-# LINE 137 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1642 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 319 "AttributeGrammar.ag" #-}
              ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1647 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 320 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1652 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 321 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1657 "AttributeGrammar.hs" #-}
              )
         ( _sizeIprecedence,_sizeIpretty) =
             size_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _lhsOinit =
             ({-# LINE 140 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1675 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 323 "AttributeGrammar.ag" #-}
              ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1680 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 324 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1685 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 325 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1690 "AttributeGrammar.hs" #-}
              )
         ( _ptrIprecedence,_ptrIpretty) =
             ptr_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let _lhsOinit :: Int
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
              {-# LINE 1711 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 327 "AttributeGrammar.ag" #-}
              ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1716 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 328 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1721 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 329 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1726 "AttributeGrammar.hs" #-}
              )
         ( _ptrIprecedence,_ptrIpretty) =
             ptr_
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOinit =
             ({-# LINE 146 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1743 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 331 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1748 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 332 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1753 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 333 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1758 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (let _lhsOinit :: Int
         _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOinit =
             ({-# LINE 149 "AttributeGrammar.ag" #-}
              label_
              {-# LINE 1771 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 335 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1776 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 336 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1781 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 337 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1786 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty))