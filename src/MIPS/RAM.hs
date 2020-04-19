module MIPS.RAM where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type

type MemAddr = Unsigned 32

instrRAM :: HiddenClockResetEnable dom
    => Signal dom MemAddr
    -> Signal dom Instruction
instrRAM x = decodeTyped . decodeFormat <$> ((blockRamFile d512 "instructions.bin") x $ pure Nothing)
