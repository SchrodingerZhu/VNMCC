{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

{-|
  Module      : MIPS.Instruction.Format
  Description : This module defines four types of MIPS binary code format.
  License     : MIT
  Maintainer  : i@zhuyi.fan
-}
module MIPS.Instruction.Format where

import Clash.Prelude

-- | The Format of MIPS Instructions
data Format            
  = NoType             -- ^ NoType (Specialized for NOP Instruction)
  | RType              -- ^ R-Type Instruction Format
      (BitVector 6)    -- ^ Operation Code
      (BitVector 5)    -- ^ Register S
      (BitVector 5)    -- ^ Register T
      (BitVector 5)    -- ^ Register D
      (BitVector 5)    -- ^ Extra Infomation for Shifting Amount
      (BitVector 6)    -- ^ Function Code
  | IType              -- ^ I-Type Instruction Format
      (BitVector 6)    -- ^ Operation Code
      (BitVector 5)    -- ^ Register S
      (BitVector 5)    -- ^ Register T
      (BitVector 16)   -- ^ Immediate Value
  | JType              -- ^ J-Type Instruction Format 
      (BitVector 6)    -- ^ Operation Code
      (BitVector 26)   -- ^ Jump Target
  deriving Show


{-|
  Transform raw instruction binary code into instruction format types.
  This function should be used together with `decodeTyped` to generate the final instruction types.
-}
decodeFormat :: BitVector 32 -> Format
decodeFormat 0 = NoType
decodeFormat vec =
  let opcode = slice d31 d26 vec
   in case opcode of
        0 ->
          pure RType <*> (slice d31 d26) <*> (slice d25 d21) <*> (slice d20 d16) <*>
          (slice d15 d11) <*>
          (slice d10 d6) <*>
          (slice d5 d0) $
          vec
        code
          | code == 0b000010 || code == 0b000011 ->
            pure JType <*> (slice d31 d26) <*> (slice d25 d0) $ vec
        _ ->
          pure IType <*> (slice d31 d26) <*> (slice d25 d21) <*> (slice d20 d16) <*>
          (slice d15 d0) $
          vec
