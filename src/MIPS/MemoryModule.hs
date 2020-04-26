module MIPS.MemoryModule where
import Clash.Prelude
import MIPS.ArithmeticModule
import MIPS.RAM
import MIPS.ControlUnit
import MIPS.RegisterFile
import Control.Monad.State
import MIPS.Utils.State
import MIPS.Forward
import MIPS.RAM
memState' :: (ALUOutput, Bool) -> State ALUOutput ALUOutput
memState' (_,     True) = do
    let res = (Nothing, MemNone', 0, Nothing)
    put res
    return res

memState' (state, _) = do
    res <- get
    put state
    return res


memState :: HiddenClockResetEnable dom => Signal dom (ALUOutput, Bool) ->  Signal dom ALUOutput
memState = asStateM memState' (Nothing, MemNone', 0, Nothing)


type MemOutput = 
    (
        Maybe (Unsigned 32)                 -- branch target
    ,   Maybe (RegNo, Reg)                  -- write pair
    )

{-# ANN memoryModule (Synthesize {
        t_name = "MemoryModule",
        t_inputs = [PortName "CLOCK",
            PortName "RESET",
            PortName "ENABLE",
            PortProduct "MMI" [PortName "WRITE_REG", 
                PortName "MEM_OP", 
                PortName "RESULT", 
                PortName "BRANCH_TARGET"],
            PortName "STALL"
           ],
        t_output = PortProduct "MMO" [
            PortName "BRANCH", 
            PortName "WRITE_PAIR"]
    }) #-}
memoryModule :: Clock System
   -> Reset System
   -> Enable System
   -> Signal System ALUOutput
   -> Signal System Bool
   -> Signal System MemOutput
memoryModule clk rst enable aluOut stall = 
    let stateMachine = (exposeClockResetEnable memState) clk rst enable
        (writeReg, memOp, aluRes, br) = unbundle $ stateMachine $ bundle (aluOut, stall)


        
        writeInfoSolver _       _             True = Nothing
        writeInfoSolver address (MemWrite' v) _    = Just (unpack address, v)
        writeInfoSolver _       _     _         = Nothing
        writeInfo                               = writeInfoSolver <$> aluRes <*> memOp <*> stall
        memData                                 = mainRAM clk rst enable (unpack <$> aluRes) writeInfo
        
        writePair (Just no) _ MemLoad' res = Just (no, res)
        writePair (Just no) res _ _        = Just (no, res)
        writePair _  _  _     _            = Nothing
        writePair' = writePair <$> writeReg <*> aluRes <*> memOp <*> memData

        check True _  = (Nothing, Nothing)
        check _     x  = x

    in check <$> stall <*> bundle (br, writePair')
