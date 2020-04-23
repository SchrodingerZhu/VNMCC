module MIPS.RAM where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type

type MemAddr = Unsigned 32
type MemoryBlock = BitVector 32


instrRAM :: HiddenClockResetEnable dom
    => Signal dom MemAddr
    -> Signal dom Instruction
instrRAM x = decodeTyped . decodeFormat <$> ((blockRamFile d512 "instructions.bin") x $ pure Nothing)


instantMem :: Vec 512 MemoryBlock -> (MemAddr, Maybe (MemAddr, BitVector 32)) -> (Vec 512 MemoryBlock, MemoryBlock) 
instantMem state (addr, Nothing) = (state, state !! (addr `unsafeShiftR` 2))
instantMem state (addr, Just (waddr, wdata)) = (state', block)
    where
        block = state !! (addr `unsafeShiftR` 2)
        state' = replace (waddr `unsafeShiftR` 2) wdata state

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
mainRAM clk rst en addr update = (exposeClockResetEnable memState) clk rst en $ bundle (addr, update)
    where
        memState = mealy instantMem $ replicate d512 0
