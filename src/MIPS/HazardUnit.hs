module MIPS.HazardUnit where

import Clash.Prelude
import MIPS.ArithmeticModule
import MIPS.ControlUnit

type HazardInput = Maybe (Unsigned 32) -- branching target    

{-# ANN hazardUnit
          (Synthesize{t_name = "HazardUnit", t_inputs = [PortName "BRANCH"],
                      t_output = PortName "STALL"})
        #-}

hazardUnit :: HazardInput -> Bool
hazardUnit (Just _) = True
hazardUnit _ = False
