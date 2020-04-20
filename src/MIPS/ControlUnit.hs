module MIPS.ControlUnit where
import           Clash.Prelude
import           MIPS.Instruction.Format
import           MIPS.Instruction.Type

type Stall = Maybe (Unsigned 2)

data MemoryOperation = MemNone
    | MemLoad
    | MemWrite
    deriving Generic
    deriving NFDataX
    deriving Show

data BranchFlag = NoBranch
    | BranchEQ (BitVector 32)
    | BranchNE (BitVector 32)
    | Jump
    deriving Generic
    deriving NFDataX

data ALUOperation = ALUAdd Bool
    | ALUSub Bool
    | ALUAnd
    | ALUNor
    | ALUOr
    | ALUXor
    | ALUSet Bool
    | ALUShiftL
    | ALUShiftR Bool
    | ALUNone
    deriving Generic
    deriving NFDataX


writeRegister :: HiddenClockResetEnable dom
    => Signal dom Instruction
    -> Signal dom (Maybe (Unsigned 5))
writeRegister = fmap writeRegister'
    where
        writeRegister' inst = case inst of
             ADD   _  _  rd -> Just rd
             ADDI  _  rt _  -> Just rt
             ADDU  _  _  rd -> Just rd
             ADDIU _  rt _  -> Just rt
             SUB   _  _  rd -> Just rd
             SUBU  _  _  rd -> Just rd
             AND   _  _  rd -> Just rd
             ANDI  _  rt  _ -> Just rt
             NOR   _  _  rd -> Just rd
             OR    _  _  rd -> Just rd
             ORI   _  rt  _ -> Just rt
             XOR   _  _  rd -> Just rd
             XORI  _  rt  _ -> Just rt
             SLT   _  _  rd -> Just rd
             SLTI  _  rt  _ -> Just rt
             SLTU  _  _  rd -> Just rd
             SLTIU _  rt  _ -> Just rt
             SLL   rd _   _ -> Just rd
             SRL   rd _   _ -> Just rd
             SRA   rd _   _ -> Just rd
             SLLV  _  _  rd -> Just rd
             SRLV  _  _  rd -> Just rd
             SRAV  _  _  rd -> Just rd
             LW    _  rt  _ -> Just rt
             JAL   _        -> Just 31
             _              -> Nothing

memoryOperation :: HiddenClockResetEnable dom
    => Signal dom Instruction
    -> Signal dom MemoryOperation
memoryOperation = fmap memoryOperation'
    where
        memoryOperation' inst = case inst of
            LW _ _ _ -> MemLoad
            SW _ _ _ -> MemWrite
            _        -> MemNone

branchFlag :: HiddenClockResetEnable dom
    => Signal dom Instruction
    -> Signal dom BranchFlag
branchFlag = fmap branchFlag'
    where
        branchFlag' inst = case inst of
            BEQ _ _ x -> BranchEQ (pack $ extend x)
            BNE _ _ x -> BranchNE (pack $ extend x)
            JR  _     -> Jump
            J   _     -> Jump
            JAL _     -> Jump
            _         -> NoBranch

dispatch :: HiddenClockResetEnable dom
    => Signal dom Instruction
    -> Signal dom ALUOperation
dispatch = fmap dispatch'
    where
        dispatch' inst = case inst of
            ADD   _ _ _ -> ALUAdd True
            ADDI  _ _ _ -> ALUAdd True
            ADDU  _ _ _ -> ALUAdd False
            SUB   _ _ _ -> ALUSub True
            SUBU  _ _ _ -> ALUSub False
            AND   _ _ _ -> ALUAnd
            ANDI  _ _ _ -> ALUAnd
            NOR   _ _ _ -> ALUNor
            OR    _ _ _ -> ALUOr
            ORI   _ _ _ -> ALUOr
            XOR   _ _ _ -> ALUXor
            XORI  _ _ _ -> ALUXor
            BEQ   _ _ _ -> ALUXor
            BNE   _ _ _ -> ALUXor
            SLT   _ _ _ -> ALUSet True
            SLTI  _ _ _ -> ALUSet True
            SLTU  _ _ _ -> ALUSet False
            SLTIU _ _ _ -> ALUSet False
            LW    _ _ _ -> ALUAdd True
            SW    _ _ _ -> ALUAdd True
            SLL   _ _ _ -> ALUShiftL
            SLLV  _ _ _ -> ALUShiftL
            SRL   _ _ _ -> ALUShiftR False
            SRLV  _ _ _ -> ALUShiftR False
            SRA   _ _ _ -> ALUShiftR True
            SRAV  _ _ _ -> ALUShiftR False
            JR    _     -> ALUOr
            J     _     -> ALUOr
            JAL   _     -> ALUOr
            _           -> ALUNone

immediateValue :: HiddenClockResetEnable dom
    => Signal dom Instruction
    -> Signal dom (Maybe (BitVector 32))
immediateValue = fmap immediateValue'
    where
        immediateValue' inst = case inst of
            ADDI   _ _ x -> Just (pack $ extend x)
            ADDIU  _ _ x -> Just (pack $ extend x)
            ANDI   _ _ x -> Just (extend x)
            ORI    _ _ x -> Just (extend x)
            XORI   _ _ x -> Just (extend x)
            SLTI   _ _ x -> Just (pack $ extend x)
            SLTIU  _ _ x -> Just (pack $ extend x)
            LW     _ _ x -> Just (pack $ extend x)
            SW     _ _ x -> Just (pack $ extend x)
            SLL    _ _ x -> Just (pack $ extend x)
            SRL    _ _ x -> Just (pack $ extend x)
            SRA    _ _ x -> Just (pack $ extend x)
            JAL    x     -> Just (pack $ extend x)
            J      x     -> Just (pack $ extend x)
            _            -> Nothing


{-# ANN controlUnit (Synthesize {
    t_name = "ControlUnit",
    t_inputs = [PortName "CLOCK", PortName "RESET", PortName "ENABLE", PortName "Instruction"],
    t_output = PortProduct "CTL" [PortName "WRITE", PortName "MEM", PortName "BRANCH_FLAG", PortName "ALU", PortName "IMM"]
}) #-}
controlUnit :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System Instruction               -- instruction
    -> ( Signal System (Maybe (Unsigned 5))    -- write register
       , Signal System MemoryOperation         -- memory
       , Signal System BranchFlag              -- branch flag
       , Signal System ALUOperation            -- ALU control
       , Signal System (Maybe (BitVector 32))) -- immediate value
controlUnit = exposeClockResetEnable $ pure (,,,,)
    <*> writeRegister
    <*> memoryOperation
    <*> branchFlag
    <*> dispatch
    <*> immediateValue



