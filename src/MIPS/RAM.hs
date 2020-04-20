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
mainRAM clk rst en addr update = (exposeClockResetEnable $ blockRam $ replicate d1024 0) clk rst en ((`unsafeShiftR` 2) <$> addr) update'
    where
        update' = updater <$> update
        updater Nothing = Nothing
        updater (Just (addr, block)) = Just (addr `unsafeShiftR` 2, block) 