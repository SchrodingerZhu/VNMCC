module MIPS.PC where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type
import           MIPS.HazardUnit.Class
import           MIPS.RAM


type PCInput = Maybe (Unsigned 32) -- branch or not             


programCounterT :: Unsigned 32 -> PCInput -> (Unsigned 32, Unsigned 32)
programCounterT state  (Just  t) =  (t + 1,                     t)
programCounterT state          _ =  (state + 1,             state)

programCounter :: HiddenClockResetEnable dom
  => Signal dom PCInput
  -> Signal dom (Unsigned 32)
programCounter = mealy programCounterT 0

{-# ANN pcModule (Synthesize {
    t_name = "InstructionModule",
    t_inputs = [PortName "CLOCK", PortName "RESET", PortName "ENABLE", PortName "STALL", PortName "BRANCH"],
    t_output = PortProduct "PC" [PortName "INSTRUCTION", PortName "VALUE"]
}) #-}

pcModule :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System StallInfo
  -> Signal System (Maybe (Unsigned 32))
  -> Signal System (Instruction, (Unsigned 32))
pcModule clk rst enable stall br = bundle (instr, next)
    where
      programCounter'      = (exposeClockResetEnable programCounter) clk rst enable
      next                 = programCounter' $ br
      ram                  = instrRAM clk rst enable next
      ram'                 = decodeTyped . decodeFormat <$>  ram
      instr' op        ram = 
        case op of
           Normal    -> ram
           Flush     -> NOP
      instr               = instr' <$> stall <*> ram'




