module MIPS.PC where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type
import           MIPS.RAM

programCounterT :: Unsigned 32 -> Maybe (Unsigned 32) -> (Unsigned 32, (Unsigned 32, Unsigned 32))
programCounterT state (Just t) = (t, (state, state + 1))
programCounterT state Nothing  = (state + 1, (state, state + 1))

programCounter :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Unsigned 32))
  -> Signal dom (Unsigned 32, Unsigned 32)
programCounter = mealy programCounterT 0

{-# ANN pcModule (Synthesize {
    t_name = "InstructionModule",
    t_inputs = [PortName "CLOCK", PortName "RESET", PortName "ENABLE", PortName "BRANCH"],
    t_output = PortProduct "PC" [PortName "INSTRUCTION", PortName "VALUE"]
}) #-}
pcModule :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Maybe (Unsigned 32))
  -> Signal System (Instruction, (Unsigned 32))
pcModule = exposeClockResetEnable instructor
    where
        instructor x =
          let (current, next)  = unbundle $ programCounter x
          in bundle (instrRAM current, next)




