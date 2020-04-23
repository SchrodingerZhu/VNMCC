module MIPS.HazardUnit where
import Clash.Prelude
import MIPS.ControlUnit
import MIPS.ArithmeticModule
import MIPS.HazardUnit.Class
{-
    We have three kinds of Hazards that require stalling the pipeline:
    - load hazard for 1 cycle only for the PC part
    - jump hazard for 3 cycle
    - branch hazard for 3 cycle
-}

type HazardInput = (
      Maybe (Unsigned 5)  -- write  register 
    , MemoryOperation'     -- memory operation
    , Unsigned 5          -- rs number
    , Unsigned 5          -- rt number
    , Maybe (Unsigned 32) -- branching target    
    )

-- Stall Type A -> for branching problem
-- If a branch happens in the write back part,
-- the data in the decode part and execution and memory is already invalid.
-- Hence, it is actually simple, just flush away all the data and refresh the CPU.


-- Stall Type B -> for load hazard
-- when stalling, we stop the state changing in the decode part and PC part
-- the decode part will behaves like a nop
-- if branching or jumping is also happening here, there is no problem
-- since the current thing will be flushed

-- As for the priority, we will check type A first



{-# ANN hazardUnit (Synthesize {
    t_name = "HazardUnit",
    t_inputs = [PortProduct "HZ" [
        PortName "WRITE",
        PortName "MEMOP",
        PortName "RS",
        PortName "RT",
        PortName "BRANCH"]],
    t_output = PortName "STALL"
}) #-}
hazardUnit :: HazardInput -> StallInfo
hazardUnit (_, _, _, _, Just _) = Flush
hazardUnit _                          = Normal




