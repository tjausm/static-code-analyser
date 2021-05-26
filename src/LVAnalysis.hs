module LVAnalysis where
import AttributeGrammar
import MFP
import Data.Set as S

lvL :: S.Set String ->  L (S.Set String)
lvL = MkLattice S.union

lvFancyF :: FancyF (S.Set String)
lvFancyF = undefined

lvF :: [Flow] -> F
lvF = MkFlow Backward
