module MIPS.Instruction.Type where
import           Clash.Prelude
import qualified MIPS.Instruction.Format as F

type Register = Unsigned 5

data Instruction = ADD Register Register Register
    | ADDI Register Register (Signed 16)
    | ADDU Register Register Register
    | ADDIU Register Register (Unsigned 16)
    | SUB Register Register Register
    | SUBU Register Register Register
    | AND Register Register Register
    | ANDI Register Register (BitVector 16)
    | NOR Register Register Register
    | OR Register Register Register
    | ORI Register Register (BitVector 16)
    | XOR Register Register Register
    | XORI Register Register (BitVector 16)
    | BEQ Register Register (Signed 16)
    | BNE Register Register (Signed 16)
    | SLT Register Register Register
    | SLTI Register Register (Signed 16)
    | SLTU Register Register Register
    | SLTIU Register Register (Unsigned 16)
    | LW Register Register (Signed 16)
    | SW Register Register (Signed 16)
    | SLL Register Register (Unsigned 5)
    | SRL Register Register (Unsigned 5)
    | SRA Register Register (Unsigned 5)
    | SLLV Register Register Register
    | SRLV Register Register Register
    | SRAV Register Register Register
    | J (Unsigned 26)
    | JAL (Unsigned 26)
    deriving Show
    deriving Generic
    deriving NFDataX




decode :: BitVector 32 -> Instruction
decode vec = decodeTyped $ F.decodeFormat vec

decodeTyped :: F.Format -> Instruction
decodeTyped (F.RType 0 rs rt rd sa fn) =
    case fn of
        0b100000 -> (makeType ADD)     (rs, rt, rd)
        0b100001 -> (makeType ADDU)    (rs, rt, rd)
        0b100100 -> (makeType AND)     (rs, rt, rd)
        0b100010 -> (makeType SUB)     (rs, rt, rd)
        0b100011 -> (makeType SUBU)    (rs, rt, rd)
        0b100111 -> (makeType NOR)     (rs, rt, rd)
        0b100101 -> (makeType OR)      (rs, rt, rd)
        0b100110 -> (makeType XOR)     (rs, rt, rd)
        0b101010 -> (makeType SLT)     (rs, rt, rd)
        0b101011 -> (makeType SLTU)    (rs, rt, rd)
        0b000000 -> (makeType' SLL)    (rd, rt, sa)
        0b000010 -> (makeType' SRL)    (rd, rt, sa)
        0b000011 -> (makeType' SRA)    (rd, rt, sa)
        0b000100 -> (makeType SLLV)    (rs, rt, rd)
        0b000110 -> (makeType SRLV)    (rs, rt, rd)
        0b000111 -> (makeType SRAV)    (rs, rt, rd)
    where
        makeType func = pure func <*> unpack . t1 <*> unpack . t2 <*> unpack . t3
        makeType' func = pure func <*> unpack . t1 <*> unpack . t2 <*> unpack . t3
        makeType2 func = pure func <*> unpack . fst <*> unpack . snd
        t1 (x, _, _) = x
        t2 (_, y, _) = y
        t3 (_, _, z) = z

decodeTyped (F.IType op rs rt imm) =
    case op of
        0b001000 -> (makeType ADDI)   (rs, rt, imm)
        0b001001 -> (makeType ADDIU)  (rs, rt, imm)
        0b001100 -> (makeType ANDI)   (rs, rt, imm)
        0b001101 -> (makeType ORI)    (rs, rt, imm)
        0b001110 -> (makeType XORI)   (rs, rt, imm)
        0b001010 -> (makeType SLTI)   (rs, rt, imm)
        0b001011 -> (makeType SLTIU)  (rs, rt, imm)
        0b000100 -> (makeType BEQ)    (rs, rt, imm)
        0b000101 -> (makeType BNE)    (rs, rt, imm)
        0b100011 -> (makeType LW)     (rs, rt, imm)
        0b101011 -> (makeType SW)     (rs, rt, imm)
    where
        makeType func = pure func <*> unpack . t1 <*> unpack . t2 <*> unpack . t3
        t1 (x, _, _) = x
        t2 (_, y, _) = y
        t3 (_, _, z) = z

decodeTyped (F.JType op target) =
    case op of
        0b000010 -> J   (unpack target)
        0b000011 -> JAL (unpack target)

