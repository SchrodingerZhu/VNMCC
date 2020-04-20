module MIPS.HazardUnit where
import Clash.Prelude
import MIPS.ControlUnit

{-
    We have three kinds of Hazards that require stalling the pipeline:
    - load hazard for 1 cycle only for the PC part
    - jump hazard for 3 cycle
    - branch hazard for 3 cycle
-}
-- type HazardInput = (
--       Maybe (Unsigned 5) -- write  Register 
--     , MemoryOperation    -- memory Operation
--     , Unsigned 5         
-- )

-- type Hazard
-- hazardUnit ::  -- writeRegister
--     ->           -- 

