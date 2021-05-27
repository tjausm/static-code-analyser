

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 12 "AttributeGrammar.hs" #-}

{-# LINE 402 "AttributeGrammar.ag" #-}

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

{-# LINE 564 "AttributeGrammar.ag" #-}

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

{-# LINE 659 "AttributeGrammar.ag" #-}

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
type T_BExpr = ( (S.Set String),Int,String)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {freeVars_Syn_BExpr :: (S.Set String),precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty) = sem
     in  (Syn_BExpr _lhsOfreeVars _lhsOprecedence _lhsOpretty))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOpretty =
             ({-# LINE 690 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 112 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 691 "AttributeGrammar.ag" #-}
              10
              {-# LINE 117 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 122 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _lhsOpretty =
             ({-# LINE 693 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 134 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 694 "AttributeGrammar.ag" #-}
              10
              {-# LINE 139 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 144 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
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
             ({-# LINE 467 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 163 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 696 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 168 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 697 "AttributeGrammar.ag" #-}
              4
              {-# LINE 173 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
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
             ({-# LINE 469 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 196 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 699 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 201 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 700 "AttributeGrammar.ag" #-}
              4
              {-# LINE 206 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
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
             ({-# LINE 471 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 229 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 702 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 234 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 703 "AttributeGrammar.ag" #-}
              4
              {-# LINE 239 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
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
             ({-# LINE 473 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 262 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 705 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 267 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 706 "AttributeGrammar.ag" #-}
              4
              {-# LINE 272 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
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
             ({-# LINE 475 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 295 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 708 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 300 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 709 "AttributeGrammar.ag" #-}
              4
              {-# LINE 305 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 711 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 328 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 712 "AttributeGrammar.ag" #-}
              4
              {-# LINE 333 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              (S.union _leftIfreeVars _rightIfreeVars)
              {-# LINE 338 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 714 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 361 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 715 "AttributeGrammar.ag" #-}
              3
              {-# LINE 366 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              (S.union _leftIfreeVars _rightIfreeVars)
              {-# LINE 371 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _leftIfreeVars :: (S.Set String)
         _leftIprecedence :: Int
         _leftIpretty :: String
         _rightIfreeVars :: (S.Set String)
         _rightIprecedence :: Int
         _rightIpretty :: String
         _lhsOpretty =
             ({-# LINE 717 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 394 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 718 "AttributeGrammar.ag" #-}
              2
              {-# LINE 399 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              (S.union _leftIfreeVars _rightIfreeVars)
              {-# LINE 404 "AttributeGrammar.hs" #-}
              )
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: (S.Set String)
         _valIfreeVars :: (S.Set String)
         _valIprecedence :: Int
         _valIpretty :: String
         _lhsOpretty =
             ({-# LINE 720 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 423 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 721 "AttributeGrammar.ag" #-}
              10
              {-# LINE 428 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 433 "AttributeGrammar.hs" #-}
              )
         ( _valIfreeVars,_valIprecedence,_valIpretty) =
             val_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
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
    (let _bExprIfreeVars :: (S.Set String)
         _bExprIprecedence :: Int
         _bExprIpretty :: String
         ( _bExprIfreeVars,_bExprIprecedence,_bExprIpretty) =
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
              {-# LINE 502 "AttributeGrammar.hs" #-}
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
              {-# LINE 521 "AttributeGrammar.hs" #-}
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
         _program'IlvLambda :: ( Int -> (LVSet-> LVSet) )
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
         _exprIfreeVars :: (S.Set String)
         _exprIprecedence :: Int
         _exprIpretty :: String
         _lhsOpretty =
             ({-# LINE 725 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 573 "AttributeGrammar.hs" #-}
              )
         ( _exprIfreeVars,_exprIprecedence,_exprIpretty) =
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
             ({-# LINE 727 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 588 "AttributeGrammar.hs" #-}
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
             ({-# LINE 733 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 620 "AttributeGrammar.hs" #-}
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
             ({-# LINE 731 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 633 "AttributeGrammar.hs" #-}
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
             ({-# LINE 667 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 711 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 668 "AttributeGrammar.ag" #-}
              10
              {-# LINE 716 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              S.empty
              {-# LINE 721 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOfreeVars :: (S.Set String)
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars =
             ({-# LINE 456 "AttributeGrammar.ag" #-}
              S.singleton name_
              {-# LINE 733 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 670 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 738 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 671 "AttributeGrammar.ag" #-}
              10
              {-# LINE 743 "AttributeGrammar.hs" #-}
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
             ({-# LINE 458 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 762 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 673 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 767 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 674 "AttributeGrammar.ag" #-}
              6
              {-# LINE 772 "AttributeGrammar.hs" #-}
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
             ({-# LINE 460 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 795 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 676 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 800 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 677 "AttributeGrammar.ag" #-}
              6
              {-# LINE 805 "AttributeGrammar.hs" #-}
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
             ({-# LINE 462 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 828 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 679 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 833 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 680 "AttributeGrammar.ag" #-}
              7
              {-# LINE 838 "AttributeGrammar.hs" #-}
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
             ({-# LINE 464 "AttributeGrammar.ag" #-}
              S.union _leftIfreeVars _rightIfreeVars
              {-# LINE 861 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 682 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 866 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 683 "AttributeGrammar.ag" #-}
              7
              {-# LINE 871 "AttributeGrammar.hs" #-}
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
             ({-# LINE 685 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 890 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 686 "AttributeGrammar.ag" #-}
              10
              {-# LINE 895 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 900 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 500 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 939 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 501 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 944 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 502 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 949 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1003 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   [labelReturn_]
                   {-# LINE 1008 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _statIflow
                   {-# LINE 1013 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 262 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1018 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 321 "AttributeGrammar.ag" #-}
                   M.singleton name_ (labelEntry_, labelReturn_)
                   {-# LINE 1023 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1028 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   L.nub (map (\x -> name_ ++ x) _statIvars)
                   {-# LINE 1033 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 597 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelReturn_ ++ ";"]
                   {-# LINE 1040 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 496 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1079 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 479 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1084 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 479 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1089 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 479 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1094 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 494 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1109 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 479 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1114 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1162 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   _hdIinterflow ++ _tlIinterflow
                   {-# LINE 1167 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 328 "AttributeGrammar.ag" #-}
                   M.union _hdIprocMapCollect _tlIprocMapCollect
                   {-# LINE 1172 "AttributeGrammar.hs" #-}
                   )
              _hdOprocMapPassDown =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1177 "AttributeGrammar.hs" #-}
                   )
              _tlOprocMapPassDown =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1182 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 361 "AttributeGrammar.ag" #-}
                   L.nub (_hdIvars ++ _tlIvars)
                   {-# LINE 1187 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 593 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1192 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1210 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 266 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1215 "AttributeGrammar.hs" #-}
                   )
              _lhsOprocMapCollect =
                  ({-# LINE 326 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1220 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 359 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1225 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 591 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1230 "AttributeGrammar.hs" #-}
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
             ({-# LINE 488 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1265 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 489 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1270 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 490 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 1275 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ([Int]),([Flow]),Int,([(Int, Int, Int, Int)]),(M.Map Int (S.Set String)),(M.Map Int (S.Set String)),( Int -> (LVSet-> LVSet) ),String,( M.Map String (Int, Int) ),([String]))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ([Int]),flow_Syn_Program' :: ([Flow]),init_Syn_Program' :: Int,interflow_Syn_Program' :: ([(Int, Int, Int, Int)]),lvGen_Syn_Program' :: (M.Map Int (S.Set String)),lvKill_Syn_Program' :: (M.Map Int (S.Set String)),lvLambda_Syn_Program' :: ( Int -> (LVSet-> LVSet) ),pretty_Syn_Program' :: String,procMapCollect_Syn_Program' :: ( M.Map String (Int, Int) ),vars_Syn_Program' :: ([String])}
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
         _lhsOlvLambda :: ( Int -> (LVSet-> LVSet) )
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
              {-# LINE 1334 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 154 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1339 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _statIflow ++ _procsIflow
              {-# LINE 1344 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 258 "AttributeGrammar.ag" #-}
              _statIinterflow ++ _procsIinterflow
              {-# LINE 1349 "AttributeGrammar.hs" #-}
              )
         _lhsOprocMapCollect =
             ({-# LINE 315 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1354 "AttributeGrammar.hs" #-}
              )
         _statOprocMapPassDown =
             ({-# LINE 316 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1359 "AttributeGrammar.hs" #-}
              )
         _procsOprocMapPassDown =
             ({-# LINE 317 "AttributeGrammar.ag" #-}
              _procsIprocMapCollect
              {-# LINE 1364 "AttributeGrammar.hs" #-}
              )
         _lhsOvars =
             ({-# LINE 350 "AttributeGrammar.ag" #-}
              L.nub (_statIvars ++ _procsIvars)
              {-# LINE 1369 "AttributeGrammar.hs" #-}
              )
         _lhsOlvLambda =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              genLambda _statIlvGen _statIlvKill
              {-# LINE 1374 "AttributeGrammar.hs" #-}
              )
         _lhsOlvKill =
             ({-# LINE 426 "AttributeGrammar.ag" #-}
              _statIlvKill
              {-# LINE 1379 "AttributeGrammar.hs" #-}
              )
         _lhsOlvGen =
             ({-# LINE 442 "AttributeGrammar.ag" #-}
              _statIlvGen
              {-# LINE 1384 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 587 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1389 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 506 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1456 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 507 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1461 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 510 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1481 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 511 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1486 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 512 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1491 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 513 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1496 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 516 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1516 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 517 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1521 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 518 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1526 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 521 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1542 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 522 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1547 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 525 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1560 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 526 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1565 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 529 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1578 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 530 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1583 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 533 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1602 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 534 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1607 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 535 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1612 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 479 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1617 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 538 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1634 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 539 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1639 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 542 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1651 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 543 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1656 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 546 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1669 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 547 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1674 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 550 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1685 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 551 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1690 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOlabel =
                  ({-# LINE 554 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1701 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 555 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1706 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1778 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 162 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 1783 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 216 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1788 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 272 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1793 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 365 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1798 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 603 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 1803 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 604 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1808 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 605 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1813 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1818 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Skip'.lhs.lvGen"
                   {-# LINE 1823 "AttributeGrammar.hs" #-}
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
              _condIfreeVars :: (S.Set String)
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
                   {-# LINE 1871 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   _stat1Ifinal ++ _stat2Ifinal
                   {-# LINE 1876 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 219 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _stat1Iinit) : Intra (labelc_, _stat2Iinit) : _stat1Iflow ++ _stat2Iflow
                   {-# LINE 1881 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 1886 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1891 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 335 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1896 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 1901 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 432 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 1906 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 448 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ _condIfreeVars) $ M.union  _stat1IlvGen _stat2IlvGen
                   {-# LINE 1911 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 607 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 1922 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 614 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1927 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 615 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1932 "AttributeGrammar.hs" #-}
                   )
              ( _condIfreeVars,_condIprecedence,_condIpretty) =
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
              _condIfreeVars :: (S.Set String)
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
                   {-# LINE 1974 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 168 "AttributeGrammar.ag" #-}
                   [labelc_]
                   {-# LINE 1979 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 222 "AttributeGrammar.ag" #-}
                   Intra (labelc_, _statIinit) : map (\x -> Intra (x, labelc_)) _statIfinal ++ _statIflow
                   {-# LINE 1984 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1989 "AttributeGrammar.hs" #-}
                   )
              _statOprocMapPassDown =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 1994 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   _statIvars
                   {-# LINE 1999 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 434 "AttributeGrammar.ag" #-}
                   _statIlvKill
                   {-# LINE 2004 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 450 "AttributeGrammar.ag" #-}
                   M.union (M.singleton labelc_ _condIfreeVars) _statIlvGen
                   {-# LINE 2009 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 617 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2016 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 620 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2021 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 621 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2026 "AttributeGrammar.hs" #-}
                   )
              ( _condIfreeVars,_condIprecedence,_condIpretty) =
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
                   {-# LINE 2055 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 171 "AttributeGrammar.ag" #-}
                   [labelExit_]
                   {-# LINE 2060 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 225 "AttributeGrammar.ag" #-}
                   (\(x, y) -> Inter (labelCall_, x) : [Inter (y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 2065 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 281 "AttributeGrammar.ag" #-}
                   (\(x, y) -> [(labelCall_, x, y, labelExit_)]) (_lhsIprocMapPassDown M.! name_)
                   {-# LINE 2070 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 375 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2075 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 623 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelExit_]
                   {-# LINE 2080 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 624 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2085 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 625 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2090 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2095 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Call'.lhs.lvGen"
                   {-# LINE 2100 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2127 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2132 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 229 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2137 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2142 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 378 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2147 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 436 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ S.singleton name_
                   {-# LINE 2152 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 452 "AttributeGrammar.ag" #-}
                   M.singleton label_ $ _valIfreeVars
                   {-# LINE 2157 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 627 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2162 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 628 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2167 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 629 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2172 "AttributeGrammar.hs" #-}
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
              _valIfreeVars :: (S.Set String)
              _valIprecedence :: Int
              _valIpretty :: String
              _lhsOinit =
                  ({-# LINE 129 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2199 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 177 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2204 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 232 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2209 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2214 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 381 "AttributeGrammar.ag" #-}
                   [name_]
                   {-# LINE 2219 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 631 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2224 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 632 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2229 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 633 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2234 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2239 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.BAssign'.lhs.lvGen"
                   {-# LINE 2244 "AttributeGrammar.hs" #-}
                   )
              ( _valIfreeVars,_valIprecedence,_valIpretty) =
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
                   {-# LINE 2289 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 180 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2294 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 235 "AttributeGrammar.ag" #-}
                   _stat1Iflow ++ _stat2Iflow ++ map (\x -> Intra (x, _stat2Iinit)) _stat1Ifinal
                   {-# LINE 2299 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow ++ _stat2Iinterflow
                   {-# LINE 2304 "AttributeGrammar.hs" #-}
                   )
              _stat1OprocMapPassDown =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2309 "AttributeGrammar.hs" #-}
                   )
              _stat2OprocMapPassDown =
                  ({-# LINE 342 "AttributeGrammar.ag" #-}
                   _lhsIprocMapPassDown
                   {-# LINE 2314 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 384 "AttributeGrammar.ag" #-}
                   L.nub (_stat1Ivars ++ _stat2Ivars)
                   {-# LINE 2319 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 430 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvKill _stat2IlvKill
                   {-# LINE 2324 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 446 "AttributeGrammar.ag" #-}
                   M.union _stat1IlvGen _stat2IlvGen
                   {-# LINE 2329 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 635 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2334 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 636 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2339 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 637 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2344 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2373 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 183 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2378 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 238 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2383 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 294 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2388 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 387 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2393 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 639 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2398 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 640 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2403 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 641 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2408 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2413 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Malloc'.lhs.lvGen"
                   {-# LINE 2418 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2444 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2449 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 241 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2454 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2459 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 390 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2464 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 643 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2469 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 644 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2474 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 645 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2479 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2484 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Free'.lhs.lvGen"
                   {-# LINE 2489 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2519 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 189 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2524 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 244 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2529 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2534 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 393 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2539 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 647 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2544 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 648 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2549 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 649 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2554 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2559 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.RefAssign'.lhs.lvGen"
                   {-# LINE 2564 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2588 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 192 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2593 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 247 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2598 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2603 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 396 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2608 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 651 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2613 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 652 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2618 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 653 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2623 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2628 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Continue'.lhs.lvGen"
                   {-# LINE 2633 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2653 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   [label_]
                   {-# LINE 2658 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 250 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2663 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2668 "AttributeGrammar.hs" #-}
                   )
              _lhsOvars =
                  ({-# LINE 399 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 2673 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 655 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2678 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 656 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2683 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 657 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2688 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvKill =
                  ({-# LINE 415 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2693 "AttributeGrammar.hs" #-}
                   )
              _lhsOlvGen =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   error "missing rule: Stat'.Break'.lhs.lvGen"
                   {-# LINE 2698 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOlvGen,_lhsOlvKill,_lhsOpretty,_lhsOvars)))