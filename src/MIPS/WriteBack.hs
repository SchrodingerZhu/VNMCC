module MIPS.WriteBack where
import Clash.Prelude
import MIPS.MemoryModule
import MIPS.RAM
import MIPS.RegisterFile

writeRegister :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Unsigned 32), Maybe (RegNo, Reg))
    -> Signal dom (Maybe (Unsigned 32), Maybe (RegNo, Reg))
writeRegister = register (Nothing, Nothing)


{-# ANN writeBack (Synthesize {
        t_name = "WriteBack",
        t_inputs = [PortName "CLOCK",
            PortName "RESET",
            PortName "ENABLE",
            PortName "BRANCH",
            PortName "WRITE_PAIR"],
        t_output = PortProduct "WB" [
            PortName "BRANCH", 
            PortName "WRITE_PAIR"]
    }) #-}
writeBack :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Maybe (Unsigned 32))
    -> Signal System (Maybe (RegNo, Reg))
    -> Signal System (Maybe (Unsigned 32), Maybe (RegNo, Reg))
writeBack clk rs en br wb = writeRegister' $ bundle (br, wb)
    where
        writeRegister' = (exposeClockResetEnable writeRegister) clk rs en