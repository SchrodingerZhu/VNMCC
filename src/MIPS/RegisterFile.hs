module MIPS.RegisterFile where
import           Clash.Prelude
import           Control.Monad.State
import           MIPS.Utils.State
type Reg = BitVector 32
type RegNo = Unsigned 5

registerFileS :: (RegNo, -- rs
                  RegNo, -- rt
                  Maybe (RegNo, Reg) -- write register
                  ) -> State (Vec 32 Reg) (Reg, Reg)
registerFileS (reg0, reg1, writePair) = do
    regs <- get
    let res0 = regs !! reg0
        res1 = regs !! reg1
        newS = case writePair of
            Nothing -> regs
            Just (a, b) ->
                if a /= 0
                then replace a b regs
                else regs
    put newS
    return (res0, res1)

{-# ANN registerFile (Synthesize {
    t_name = "RegisterFile",
    t_inputs = [PortName "CLOCK", PortName "RESET", PortName "ENABLE",
        PortProduct "RF" [PortName "RS", PortName "RT", PortName "WRITE"]],
    t_output = PortProduct "RF"
        [PortName "RSV", PortName "RTV"]
}) #-}
registerFile :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (RegNo, RegNo, Maybe (RegNo, Reg))
    -> Signal System (Reg, Reg)
registerFile = exposeClockResetEnable $ asStateM registerFileS (replicate d32 0)
