{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module MIPS.Instruction.Format where

import Clash.Prelude

data Format
  = NoType
  | RType
      (BitVector 6)
      (BitVector 5)
      (BitVector 5)
      (BitVector 5)
      (BitVector 5)
      (BitVector 6)
  | IType (BitVector 6) (BitVector 5) (BitVector 5) (BitVector 16)
  | JType (BitVector 6) (BitVector 26)
  deriving (Show)

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
