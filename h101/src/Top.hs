module Top (main) where

import qualified Interpreter
import qualified Bytecode
import qualified MotivateStateMonad
import qualified OldBf
import qualified Bf
import qualified Sol
import qualified Cps
import qualified ParserCombinators
import qualified PC2
import qualified MySet
import qualified PlayMemo
import qualified InstructionSelection
import qualified MonadMasterClass
import qualified Live
import qualified Master2
import qualified Master3
import qualified RegAlloc
import qualified Lisp
import qualified PrepGreen
import qualified Fork
import qualified Green
import qualified Nite
import qualified Dft

main :: IO ()
main = do
  let _ = Interpreter.main
  let _ = Bytecode.main
  let _ = MotivateStateMonad.main
  let _ = OldBf.main
  let _ = Bf.main
  let _ = Sol.main
  let _ = Cps.main
  let _ = ParserCombinators.main
  let _ = PC2.main
  let _ = MySet.main
  let _ = PlayMemo.main
  let _ = InstructionSelection.main
  let _ = MonadMasterClass.main
  let _ = Live.main
  let _ = Master2.main
  let _ = Master3.main
  let _ = RegAlloc.main
  let _ = Lisp.main
  let _ = PrepGreen.main
  let _ = Fork.main
  let _ = Green.main
  let _ = Nite.main
  Dft.main
  pure ()
