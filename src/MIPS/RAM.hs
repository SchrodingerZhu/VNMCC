module MIPS.RAM where
import Clash.Prelude
type MemAddr = Unsigned 32


instrRAM :: HiddenClockResetEnable dom
    => Signal dom MemAddr
    -> Signal dom (BitVector 32)
instrRAM = flip (blockRam $ (replicate d512 0)) $ pure Nothing
