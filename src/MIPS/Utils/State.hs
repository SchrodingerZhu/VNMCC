module MIPS.Utils.State where
import           Clash.Prelude
import           Control.Monad.State
asStateM
  :: ( HiddenClockResetEnable dom
     , NFDataX s )
  => (i -> State s o)
  -> s
  -> (Signal dom i -> Signal dom o)
asStateM f i = mealy g i
  where
    g s x = let (o,s') = runState (f x) s
            in  (s',o)

