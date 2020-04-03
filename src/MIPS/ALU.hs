module MIPS.ALU where
import Clash.Prelude
import qualified MIPS.Instruction.Type as T
import MIPS.Utils.Arithmetic
import Clash.Explicit.Testbench
import Data.Int
-- operations
-- add       0 
-- addu      1
-- sub       2
-- subu      3
-- mult      4
-- multu     5
-- and       6
-- nor       7  
-- or        8
-- xor       9
-- [BLANK]  10
-- sll      11
-- srl      12
-- sra      13
-- slt      14
-- sltu     15
-- div      16
-- divu     17

{-# ANN controlALU (Synthesize {
    t_name = "CONTROL_ALU",
    t_inputs = [PortName "instr", PortName "reg0", PortName "reg1"],
    t_output = PortProduct "CTL" [PortName "OPT", PortName "X", PortName "Y", PortName "REG0", PortName "REG1", PortName "TARGET", PortName "Branch"]
})#-}
controlALU :: BitVector 32
    -> BitVector 32
    -> BitVector 32
    -> (BitVector 5, BitVector 32, BitVector 32, Unsigned 5, Unsigned 5, Unsigned 5, BitVector 2)
controlALU x y z =
    case T.decode x of
        T.ADD rs rt rd      ->  (0,  y, z, rs, rt, rd, 0)
        T.ADDI  rs rt imm   ->  (0,  y, pack $ extend imm, rs, 0, rt, 0)
        T.ADDU rs rt rd     ->  (1,  y, z, rs, rt, rd, 0)
        T.ADDIU rs rt imm  ->   (1,  y, pack $ extend imm, rs, 0, rt, 0)
        T.SUB rs rt rd      ->  (2,  y, z, rs, rt, rd, 0)
        T.SUBU rs rt rd     ->  (3,  y, z, rs, rt, rd, 0)
        T.MULT rs rt       ->   (4,  y, z, rs, rt, 0, 0)
        T.MULTU rs rt      ->   (5,  y, z, rs, rt, 0, 0)
        T.DIV rs rt       ->    (16, y, z, rs, rt, 0, 0)
        T.DIVU rs rt      ->    (17, y, z, rs, rt, 0, 0)
        T.AND rs rt rd      ->  (6,  y, z, rs, rt, rd, 0)
        T.ANDI rs rt imm   ->   (6,  y, extend imm, rs, 0, rt, 0)
        T.NOR rs rt rd      ->  (7,  y, z, rs, rt, rd, 0)
        T.OR rs rt rd       ->  (8,  y, z, rs, rt, rd, 0)
        T.ORI rs rt imm    ->   (8,  y, extend imm, rs, 0, rt, 0)
        T.XOR rs rt rd      ->  (9,  y, z, rs, rt, rd, 0)
        T.XORI rs rt imm   ->   (9,  y, extend imm, rs, 0, rt, 0)
        T.BEQ rs rt _    ->     (2,  y, z, rs, rt, 0, 0b01)
        T.BNE rs rt _   ->      (2,  y, z, rs, rt, 0, 0b10)
        T.SLT rs rt rd      ->  (14, y, z, rs, rt, rd, 0)
        T.SLTI rs rt imm   ->   (14, y, pack $ extend imm, rs, 0, rt, 0)
        T.SLTU rs rt rd     ->  (15, y, z, rs, rt, rd, 0)
        T.SLTIU rs rt imm  ->   (15, y, pack $ extend imm, rs, 0, rt, 0)
        T.LW rs rt imm     ->   (0,  y, pack $ extend imm, rs, 0, rt, 0)
        T.SW rs rt imm     ->   (0,  y, pack $ extend imm, rs, 0, rt, 0)
        T.SLL rs rt imm    ->   (11, z, pack $ extend imm, rt, 0, rs, 0)
        T.SLLV rs rt imm   ->   (11, z, y, rs, 0, rt, 0)
        T.SRL rs rt imm    ->   (12, z, pack $ extend imm, rs, 0, rt, 0)
        T.SRLV rs rt imm   ->   (12, z, y, rs, 0, rt, 0)
        T.SRA rs rt imm    ->   (13, z, pack $ extend imm, rs, 0, rt, 0)
        T.SRAV rs rt imm   ->   (13, z, y, rs, 0, rt, 0)

{-# ANN aluChip (Synthesize {
    t_name = "ALU",
    t_inputs = [PortName "CTL", PortName "reg0", PortName "reg1"],
    t_output = PortProduct "ALU" [
        PortName "out",
        PortName "HI",
        PortName "LOW", 
        PortName "V", 
        PortName "Z", 
        PortName "N"
    ]
})#-}
aluChip :: BitVector 5
    -> BitVector 32
    -> BitVector 32
    -> (BitVector 32, BitVector 32, BitVector 32, Bool, Bool, Bool)

aluChip 0 x y = 
    let (res, overflow) = addOverflow (unpack x) (unpack y) in
        (pack res, 0, 0, overflow, res == 0, res < 0)
aluChip 1 x y = 
        (x + y, 0, 0, False, x + y == 0, x + y < 0)
aluChip 2 x y = 
    let (res, overflow) = subOverflow (unpack x) (unpack y) in
        (pack res, 0, 0, overflow, res == 0, res < 0) 
aluChip 3 x y = 
        (x - y, 0, 0, False, x - y == 0, x - y < 0) 
aluChip 4 x y = 
        let m = extend (unpack x) :: Signed 64
            n = extend (unpack y) :: Signed 64
            res' = m * n
            res = pack $ res'
        in (0, slice d63 d32 res, slice d31 d0 res, False, res == 0, res' < 0)  
aluChip 5 x y = 
        let m = extend (unpack x) :: Unsigned 64
            n = extend (unpack y) :: Unsigned 64
            res = pack $ m * n
        in (0, slice d63 d32 res, slice d31 d0 res, False, res == 0, False)             
aluChip 6 a b = (a .&. b, 0, 0, False, a .&. b == 0, False)
aluChip 7 a b = 
    let res = complement (a .|. b)
    in (res, 0, 0, False, res == 0, False)
aluChip 8 a b = 
    let res = a .|. b
    in (res, 0, 0, False, res == 0, False)
aluChip 9 a b = 
    let res = a `xor` b
    in (res, 0, 0, False, res == 0, False)
aluChip 11 a b = 
    let res = a `shiftL` (unpack $ extend b)
    in (res, 0, 0, False, res == 0, False)   
aluChip 12 a b = 
    let res = a `unsafeShiftR` (unpack $ extend b)
    in (res, 0, 0, False, res == 0, False)
aluChip 13 a b = 
    let res = (unpack a :: Signed 32) `shiftR` (unpack $ extend b)
    in (pack res, 0, 0, False, res == 0, False) 
aluChip 14 x y = 
    let m = unpack x :: Signed 32
        n = unpack y :: Signed 32
        res = if m < n then 1 else 0
    in (res, 0, 0, False, not (m < n), False)
aluChip 15 x y = 
    let m = unpack x :: Unsigned 32
        n = unpack y :: Unsigned 32
        res = if m < n then 1 else 0
    in (res, 0, 0, False, not (m < n), False)
aluChip 16 x y = 
    let m = unpack x :: Signed 32
        n = unpack y :: Signed 32
    in (0, pack $ m `rem` n, pack $ m `quot` n, False, False, False)
aluChip 17 x y = 
    let m = unpack x :: Unsigned 32
        n = unpack y :: Unsigned 32
    in (0, pack $ m `rem` n, pack $ m `quot` n, False, False, False)
