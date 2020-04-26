module MIPS.RAM where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type

type MemAddr = Unsigned 32
type MemoryBlock = BitVector 32


instrRAM' :: HiddenClockResetEnable dom
    => Signal dom MemAddr
    -> Signal dom (BitVector 32)
instrRAM' =  (flip $ blockRamFile d512 "instructions.bin") $ pure Nothing

instrRAM :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System MemAddr
    -> Signal System (BitVector 32)
instrRAM  = exposeClockResetEnable instrRAM' 
        
{-# ANN mainRAM (Synthesize {
    t_name = "MainMemory",
    t_inputs = [
        PortName "CLOCK",
        PortName "RESET",
        PortName "ENABLE",
        PortName "FETCH_ADDRESS",
        PortName "EDIT_SERIAL"],
    t_output = PortName "DATA"
}) #-}
mainRAM :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System MemAddr
    -> Signal System (Maybe (MemAddr, MemoryBlock))
    -> Signal System MemoryBlock
mainRAM = exposeClockResetEnable $ asyncRam d512