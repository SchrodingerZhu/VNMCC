module MIPS.ALU where
import           Clash.Prelude
import           MIPS.ControlUnit
import           MIPS.Utils.Arithmetic
type ALUResult = (
        BitVector 32   -- Arithmetic Result
    ,   Bool           -- Overflow Result
    ,   Bool           -- Zero Flag
    ,   Bool)          -- Negative Flag

{-# ANN arithmeticUnit (Synthesize {
    t_name = "ArithmeticUnit",
    t_inputs = [PortName "OPERATION", PortName "OPERAND_1", PortName "OPERAND_2"],
    t_output = PortProduct "ALU" [PortName "RESULT", PortName "OVERFLOW", PortName "ZERO", PortName "NEG"]
}) #-}
arithmeticUnit :: ALUOperation
    -> BitVector 32
    -> BitVector 32
    -> ALUResult
arithmeticUnit op opr0 opr1 = (res, o, z, n)
    where
        (res, o, n) = arithmeticUnit' op
        z           = res == 0
        arithmeticUnit' (ALUAdd flag) =
            let (res, overflow) = addOverflow opr0 opr1
            in  (res, overflow && flag, bitToBool $ res ! 31)
        arithmeticUnit' (ALUSub flag) =
            let (res, overflow) = subOverflow opr0 opr1
            in  (res, overflow && flag, bitToBool $ res ! 31)
        arithmeticUnit' ALUAnd        = (opr0 .&. opr1, False, False)
        arithmeticUnit' ALUNor        = (complement $ opr0 .|. opr1, False, False)
        arithmeticUnit' ALUOr         = (opr0 .|. opr1, False, False)
        arithmeticUnit' (ALUSet flag) =
            let result = boolToBV $ if flag then
                    (unpack opr0 :: Signed 32) < (unpack opr1 :: Signed 32)
                else
                    (unpack opr0 :: Unsigned 32) < (unpack opr1 :: Unsigned 32)
            in (result, False, False)
        arithmeticUnit' ALUShiftL    = (opr0 `shiftL` (unpack $ extend opr1), False, False)
        arithmeticUnit' (ALUShiftR True)   =
            (pack $ (unpack opr0 :: Signed 32) `shiftR` (unpack $ extend opr1), False, False)
        arithmeticUnit' (ALUShiftR False)   = (opr0 `unsafeShiftR` (unpack $ extend opr1), False, False)




