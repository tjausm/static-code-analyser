

-- UUAGC 0.9.52.2 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 11 "AttributeGrammar.hs" #-}

{-# LINE 181 "AttributeGrammar.ag" #-}

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

{-# LINE 276 "AttributeGrammar.ag" #-}

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
             ({-# LINE 307 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 97 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 308 "AttributeGrammar.ag" #-}
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
             ({-# LINE 310 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 113 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 311 "AttributeGrammar.ag" #-}
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
             ({-# LINE 313 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 134 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
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
             ({-# LINE 316 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 159 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 317 "AttributeGrammar.ag" #-}
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
             ({-# LINE 319 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 184 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 320 "AttributeGrammar.ag" #-}
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
             ({-# LINE 322 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 209 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 323 "AttributeGrammar.ag" #-}
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
             ({-# LINE 325 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 234 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 326 "AttributeGrammar.ag" #-}
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
             ({-# LINE 328 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 259 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 329 "AttributeGrammar.ag" #-}
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
             ({-# LINE 331 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 284 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 332 "AttributeGrammar.ag" #-}
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
             ({-# LINE 334 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 309 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 335 "AttributeGrammar.ag" #-}
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
             ({-# LINE 337 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 331 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 338 "AttributeGrammar.ag" #-}
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
    (let _stat'IisSingle :: Bool
         _stat'IisSkip :: Bool
         _stat'Ipretty :: ( [String] )
         ( _stat'IisSingle,_stat'IisSkip,_stat'Ipretty) =
             stat'_
     in  ( ))
sem_Code_CProc :: (T_Proc') ->
                  T_Code
sem_Code_CProc proc'_ =
    (let _proc'Ipretty :: ( [String] )
         ( _proc'Ipretty) =
             proc'_
     in  ( ))
sem_Code_CProgram :: (T_Program') ->
                     T_Code
sem_Code_CProgram program'_ =
    (let _program'Ipretty :: String
         ( _program'Ipretty) =
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
             ({-# LINE 342 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 439 "AttributeGrammar.hs" #-}
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
             ({-# LINE 344 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 453 "AttributeGrammar.hs" #-}
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
             ({-# LINE 350 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 485 "AttributeGrammar.hs" #-}
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
             ({-# LINE 348 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 498 "AttributeGrammar.hs" #-}
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
             ({-# LINE 284 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 545 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 285 "AttributeGrammar.ag" #-}
              10
              {-# LINE 550 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOprecedence,_lhsOpretty))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOpretty =
             ({-# LINE 287 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 561 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 288 "AttributeGrammar.ag" #-}
              10
              {-# LINE 566 "AttributeGrammar.hs" #-}
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
             ({-# LINE 290 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 582 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 291 "AttributeGrammar.ag" #-}
              6
              {-# LINE 587 "AttributeGrammar.hs" #-}
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
             ({-# LINE 293 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 607 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 294 "AttributeGrammar.ag" #-}
              6
              {-# LINE 612 "AttributeGrammar.hs" #-}
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
             ({-# LINE 296 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 632 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 297 "AttributeGrammar.ag" #-}
              7
              {-# LINE 637 "AttributeGrammar.hs" #-}
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
             ({-# LINE 299 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 657 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 300 "AttributeGrammar.ag" #-}
              7
              {-# LINE 662 "AttributeGrammar.hs" #-}
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
             ({-# LINE 302 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 679 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 303 "AttributeGrammar.ag" #-}
              10
              {-# LINE 684 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 723 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 118 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 728 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 119 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 733 "AttributeGrammar.hs" #-}
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
type T_Proc' = ( ( [String] ))
data Inh_Proc' = Inh_Proc' {}
data Syn_Proc' = Syn_Proc' {pretty_Syn_Proc' :: ( [String] )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc') =
    (let ( _lhsOpretty) = sem
     in  (Syn_Proc' _lhsOpretty))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelReturn_ name_ inp_ out_ stat_ =
    (let _lhsOpretty :: ( [String] )
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 214 "AttributeGrammar.ag" #-}
              ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
               ++ indent _statIpretty
               ++ ["end" ++ showLabel labelReturn_ ++ ";"]
              {-# LINE 773 "AttributeGrammar.hs" #-}
              )
         ( _statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOpretty))
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
                  ({-# LINE 113 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 812 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 96 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 817 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 96 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 822 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 96 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 827 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 842 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 96 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 847 "AttributeGrammar.hs" #-}
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
         _hdIpretty :: ( [String] )
         _tlIpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 210 "AttributeGrammar.ag" #-}
              _hdIpretty ++ _tlIpretty
              {-# LINE 877 "AttributeGrammar.hs" #-}
              )
         ( _hdIpretty) =
             hd_
         ( _tlIpretty) =
             tl_
     in  ( _lhsOpretty))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (let _lhsOpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              []
              {-# LINE 890 "AttributeGrammar.hs" #-}
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
             ({-# LINE 105 "AttributeGrammar.ag" #-}
              1
              {-# LINE 925 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 106 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 930 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 107 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 935 "AttributeGrammar.hs" #-}
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
type T_Program' = ( String)
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {pretty_Syn_Program' :: String}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOpretty) = sem
     in  (Syn_Program' _lhsOpretty))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let _lhsOpretty :: String
         _procsIpretty :: ( [String] )
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 204 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 972 "AttributeGrammar.hs" #-}
              )
         ( _procsIpretty) =
             procs_
         ( _statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOpretty))
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
                  ({-# LINE 123 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1039 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 124 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1044 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 127 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1064 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 128 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1069 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 129 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1074 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 130 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1079 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 133 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1099 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 134 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1104 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1109 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 138 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1125 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 139 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1130 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1143 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 143 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1148 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1161 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1166 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 150 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1185 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 151 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1190 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1195 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 96 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1200 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 155 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1217 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 156 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1222 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1234 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 160 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1239 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 163 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1252 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 164 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1257 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 167 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1268 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 168 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1273 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 171 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1284 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 172 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1289 "AttributeGrammar.hs" #-}
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
type T_Stat' = ( Bool,Bool,( [String] ))
data Inh_Stat' = Inh_Stat' {}
data Syn_Stat' = Syn_Stat' {isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat') =
    (let ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty) = sem
     in  (Syn_Stat' _lhsOisSingle _lhsOisSkip _lhsOpretty))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOpretty =
             ({-# LINE 220 "AttributeGrammar.ag" #-}
              ["skip" ++ showLabel label_]
              {-# LINE 1352 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 221 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1357 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 222 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1362 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _condIprecedence :: Int
         _condIpretty :: String
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 224 "AttributeGrammar.ag" #-}
              ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
               ++ indent _stat1Ipretty
               ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                       [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                   ++ indent _stat2Ipretty
                   ++ (if _stat2IisSingle then [] else ["}"])
                 )
              {-# LINE 1391 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 231 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1396 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 232 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1401 "AttributeGrammar.hs" #-}
              )
         ( _condIprecedence,_condIpretty) =
             cond_
         ( _stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
             stat1_
         ( _stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
             stat2_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _condIprecedence :: Int
         _condIpretty :: String
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 234 "AttributeGrammar.ag" #-}
              ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
               ++ indent _statIpretty
               ++ (if _statIisSingle then [] else ["}"])
              {-# LINE 1428 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 237 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1433 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 238 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1438 "AttributeGrammar.hs" #-}
              )
         ( _condIprecedence,_condIpretty) =
             cond_
         ( _statIisSingle,_statIisSkip,_statIpretty) =
             stat_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelExit_ name_ params_ out_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _paramsIpretty :: String
         _lhsOpretty =
             ({-# LINE 240 "AttributeGrammar.ag" #-}
              ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
              {-# LINE 1459 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 241 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1464 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 242 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1469 "AttributeGrammar.hs" #-}
              )
         ( _paramsIpretty) =
             params_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOpretty =
             ({-# LINE 244 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1487 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 245 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1492 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 246 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1497 "AttributeGrammar.hs" #-}
              )
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOpretty =
             ({-# LINE 248 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1515 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 249 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1520 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 250 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1525 "AttributeGrammar.hs" #-}
              )
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _lhsOpretty =
             ({-# LINE 252 "AttributeGrammar.ag" #-}
              addSemicolon _stat1Ipretty ++ _stat2Ipretty
              {-# LINE 1546 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 253 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1551 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 254 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1556 "AttributeGrammar.hs" #-}
              )
         ( _stat1IisSingle,_stat1IisSkip,_stat1Ipretty) =
             stat1_
         ( _stat2IisSingle,_stat2IisSkip,_stat2Ipretty) =
             stat2_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _sizeIprecedence :: Int
         _sizeIpretty :: String
         _lhsOpretty =
             ({-# LINE 256 "AttributeGrammar.ag" #-}
              ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1576 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 257 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1581 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 258 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1586 "AttributeGrammar.hs" #-}
              )
         ( _sizeIprecedence,_sizeIpretty) =
             size_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _lhsOpretty =
             ({-# LINE 260 "AttributeGrammar.ag" #-}
              ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1603 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 261 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1608 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 262 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1613 "AttributeGrammar.hs" #-}
              )
         ( _ptrIprecedence,_ptrIpretty) =
             ptr_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOpretty =
             ({-# LINE 264 "AttributeGrammar.ag" #-}
              ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1633 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 265 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1638 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 266 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1643 "AttributeGrammar.hs" #-}
              )
         ( _ptrIprecedence,_ptrIpretty) =
             ptr_
         ( _valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOpretty =
             ({-# LINE 268 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1659 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 269 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1664 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 270 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1669 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOpretty =
             ({-# LINE 272 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1681 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 273 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1686 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 274 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1691 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty))