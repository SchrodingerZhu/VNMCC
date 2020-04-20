module MIPS.PC where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type
import           MIPS.RAM
type PCInput = 
  (  
          Bool                -- stall or not
  ,       Maybe (Unsigned 32) -- branch or not             
  )

programCounterT :: Unsigned 32 -> PCInput -> (Unsigned 32, (Unsigned 32, Unsigned 32))
programCounterT state  (True ,          _) =  (state,                 (state, state + 1))
programCounterT state  (False,  (Just  t)) =  (t,                             (t, t + 1))
programCounterT state  (False,    Nothing) =  (state + 1,             (state, state + 1))

programCounter :: HiddenClockResetEnable dom
  => Signal dom PCInput
  -> Signal dom (Unsigned 32, Unsigned 32)
programCounter = mealy programCounterT 0

{-# ANN pcModule (Synthesize {
    t_name = "InstructionModule",
    t_inputs = [PortName "CLOCK", PortName "RESET", PortName "ENABLE", PortName "STALL", PortName "BRANCH"],
    t_output = PortProduct "PC" [PortName "INSTRUCTION", PortName "VALUE"]
}) #-}
pcModule :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Bool
  -> Signal System (Maybe (Unsigned 32))
  -> Signal System (Instruction, (Unsigned 32))
pcModule = exposeClockResetEnable instructor
    where
        instructor stall x =
          let (current, next)  = unbundle $ programCounter $ bundle (stall, x)
              instr'  flag ram    = if flag then NOP else  ram
              instr               = instr' <$> stall <*> instrRAM current
          in  bundle (instr, next)




