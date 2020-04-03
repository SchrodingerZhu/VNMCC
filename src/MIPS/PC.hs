module MIPS.PC where
import Clash.Prelude
import MIPS.RAM

programCounterT :: Unsigned 32 -> Maybe (Unsigned 32) -> (Unsigned 32, Unsigned 32)
programCounterT state (Just t) = (t, state)
programCounterT state Nothing  = (state + 1, state)

programCounter :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Unsigned 32))
  -> Signal dom (Unsigned 32)
programCounter = mealy programCounterT 0

{-# ANN pcModule (Synthesize {
    t_name = "pcModule",
    t_inputs = [PortName "instr", PortName "reg0", PortName "reg1"],
    t_output = PortName "test"
}) #-}
pcModule :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Maybe (Unsigned 32))
  -> Signal System (BitVector 32)
pcModule = exposeClockResetEnable instructor
    where
        instructor = \x -> instrRAM (programCounter x)



