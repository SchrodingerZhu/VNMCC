{-# LANGUAGE LambdaCase #-}
module MIPS.DecodeModule where

import Clash.Prelude
import Control.Monad.State
import MIPS.ControlUnit
import MIPS.Instruction.Format
import MIPS.Instruction.Type
import MIPS.RegisterFile
import MIPS.Utils.State

registerPair ::
     HiddenClockResetEnable dom
  => Signal dom Instruction
  -> Signal dom (RegNo, RegNo)
registerPair = fmap $ \case
        ADD x y _ -> (x, y)
        ADDI x _ _ -> (x, 0)
        ADDU x y _ -> (x, y)
        ADDIU x _ _ -> (x, 0)
        SUB x y _ -> (x, y)
        SUBU x y _ -> (x, y)
        AND x y _ -> (x, y)
        ANDI x _ _ -> (x, 0)
        NOR x y _ -> (x, y)
        OR x y _ -> (x, y)
        ORI x _ _ -> (x, 0)
        XOR x y _ -> (x, y)
        XORI x _ _ -> (x, 0)
        BEQ x y _ -> (x, y)
        BNE x y _ -> (x, y)
        SLT x y _ -> (x, y)
        SLTI x _ _ -> (x, 0)
        SLTU x y _ -> (x, y)
        SLTIU x _ _ -> (x, 0)
        LW x _ _ -> (x, 0)
        SW x y _ -> (x, y)
        SLL _ x _ -> (x, 0)
        SRL _ x _ -> (x, 0)
        SRA _ x _ -> (x, 0)
        SLLV x y _ -> (y, x)
        SRLV x y _ -> (y, x)
        SRAV x y _ -> (y, x)
        NOP -> (0, 0)
        J _ -> (0, 0)
        JAL _ -> (0, 0)
        JR x -> (x, 0)

type DecodeModuleState = (Instruction, Unsigned 32)

decodeModuleState ::
     (Instruction, Unsigned 32, Bool)
  -> State DecodeModuleState DecodeModuleState
decodeModuleState (inst, pc, stall) = do
  case stall of
    False -> do
      state <- get
      put (inst, pc)
      return state
    True -> do
      let res = (NOP, 0)
      put res
      return res

{-# ANN decodeModule
          (Synthesize{t_name = "DecodeModule",
                      t_inputs =
                        [PortName "CLOCK", PortName "RESET", PortName "ENABLE",
                         PortName "WRITE_REG", PortName "STALL", PortName "INSTRUCTION",
                         PortName "COUNTER"],
                      t_output =
                        PortProduct "DM"
                          [PortName "WRITE", PortName "MEM", PortName "BRANCH_FLAG",
                           PortName "ALU", PortName "IMM", PortName "RS", PortName "RSV",
                           PortName "RT", PortName "RTV", PortName "COUNTER"]})
        #-}

decodeModule ::
     Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Maybe (RegNo, Reg)) -- write data
  -> Signal System Bool -- stall
  -> Signal System Instruction -- instruction
  -> Signal System (Unsigned 32) -- counter
  -> Signal System ( Maybe (Unsigned 5) -- write register
                   , MemoryOperation -- memory
                   , BranchFlag -- branch flag
                   , ALUOperation -- ALU control
                   , Maybe (BitVector 32) -- immediate value
                   , RegNo -- rs
                   , BitVector 32 -- rs value
                   , RegNo -- rd
                   , BitVector 32 -- rd value
                   , Unsigned 32 -- output counter
                    )
decodeModule clk rst enable wdata stall inst counter =
  let stateMachine =
        exposeClockResetEnable $ asStateM decodeModuleState (NOP, 0)
      (rinst, pc) =
        unbundle (stateMachine clk rst enable $ bundle (inst, counter, stall))
      regDecoder = exposeClockResetEnable registerPair
      (rs, rt) = unbundle $ regDecoder clk rst enable rinst
      (w, m, b, a, i) = controlUnit clk rst enable rinst
      (rsv, rtv) =
        unbundle (registerFile clk rst enable $ bundle (rs, rt, wdata))
   in bundle (w, m, b, a, i, rs, rsv, rt, rtv, pc)
