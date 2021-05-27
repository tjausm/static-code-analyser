{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module LVAnalysis where
import AttributeGrammar
import MFP
import Data.Set as S

instance Eq LVSet where
  (MkSet x) == (MkSet y) = x `S.isSubsetOf` y && y `S.isSubsetOf` x

instance Eq LVSet => Ord LVSet where
  (MkSet x) > (MkSet y) = y `S.isSubsetOf` x && MkSet x /= MkSet y
  (MkSet x) < (MkSet y) = x `S.isSubsetOf` y && MkSet x /= MkSet y
  (MkSet x) <= (MkSet y) = x `S.isSubsetOf` y

instance Show LVSet where
  show (MkSet x) = show x

lvL :: LVSet ->  L LVSet
lvL = MkLattice join
    where
        join (MkSet x) (MkSet y) = MkSet $ x `S.union` y


lvFancyF :: FancyF LVSet
lvFancyF = undefined

lvF :: [Flow] -> F
lvF = MkFlow Backward
