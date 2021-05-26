module Compiler where

import Parser
import Lexer
import AttributeGrammar
import PrettyPrinter
import MFP
import LVAnalysis (lvL, lvFancyF, lvF)

import qualified Data.Map.Internal.Debug as MD
import qualified Data.Map as M
import qualified Data.Set as S

compile :: String -> IO ()
compile source = do
  let program = happy $ alex source
  let synProgram  = wrap_Program  (sem_Program  program)  Inh_Program
  let program' = labelled_Syn_Program synProgram
  let synProgram' = wrap_Program' (sem_Program' program') Inh_Program'

  putStrLn ""
  putStrLn "# Program"
  putStrLn $ pretty_Syn_Program' synProgram'
  putStrLn $ printFlowList $ flow_Syn_Program' synProgram'
  putStrLn "Init"
  print (init_Syn_Program' synProgram')
  putStrLn "Final"
  print (final_Syn_Program' synProgram')
  putStrLn "Vars"
  print (vars_Syn_Program' synProgram')
  putStrLn  "\n LV Kill sets"
  putStrLn $ MD.showTree $ lvKill_Syn_Program' synProgram'
  putStrLn  "\n LV Gen sets"
  putStrLn $ MD.showTree $ lvGen_Syn_Program' synProgram'
  putStrLn  "\n LV analysis"

  let flow = flow_Syn_Program' synProgram'
  let e = final_Syn_Program' synProgram'
  let bottom = S.fromList $ vars_Syn_Program' synProgram'
  let lambdaF = lvLambda_Syn_Program' synProgram'
  let labels = genLabels flow
  let test = maximalFixedPoint (lvL bottom) lvFancyF (lvF flow) e S.empty  lambdaF labels
  putStrLn $ showMFP S.showTree test


