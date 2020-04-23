module MIPS.PC where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type
import           MIPS.HazardUnit.Class
import           MIPS.RAM


type PCInput = 
  (  
          StallInfo           -- stall or not
  ,       Maybe (Unsigned 32) -- branch or not             
  )

programCounterT :: Unsigned 32 -> PCInput -> (Unsigned 32, (Unsigned 32, Unsigned 32))
programCounterT state  (_    ,  (Just  t)) =  (t + 1,                         (t, t + 1))
programCounterT state  (_    ,          _) =  (state + 1,             (state, state + 1))

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
  -> Signal System StallInfo
  -> Signal System (Maybe (Unsigned 32))
  -> Signal System (Instruction, (Unsigned 32))
pcModule = exposeClockResetEnable instructor
    where
        instructor :: 
          HiddenClockResetEnable dom
            => Signal dom StallInfo
            -> Signal dom (Maybe (Unsigned 32))
            -> Signal dom (Instruction, (Unsigned 32))
        instructor stall br =
            let 
                (current, next)     = unbundle $ programCounter $ bundle (stall, br)
                ram                 = instrRAM current

                instr' op        ram = 
                  case op of
                    Normal    -> ram
                    Flush     -> NOP
                instr               = instr' <$> stall <*> instrRAM current
            in  bundle (instr, next)




