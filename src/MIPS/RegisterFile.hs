module MIPS.RegisterFile where
import Clash.Prelude
import Control.Monad.State
type Reg = BitVector 32
type RegNo = Unsigned 5

registerFileS :: (RegNo, RegNo, Maybe (RegNo, Reg))
    -> State (Vec 32 Reg) (Reg, Reg)
registerFileS (reg0, reg1, writePair) = do
    regs <- get
    let res0 = regs !! reg0
        res1 = regs !! reg1
        newS = case writePair of
            Nothing -> regs
            Just (a, b) -> 
                if a /= 0
                then replace a b regs
                else regs
    put newS
    return (res0, res1)

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

registerFile = asStateM registerFileS (replicate d32 0)

topEntity :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (RegNo, RegNo, Maybe (RegNo, Reg))
    -> Signal System (Reg, Reg)
topEntity = exposeClockResetEnable registerFile
