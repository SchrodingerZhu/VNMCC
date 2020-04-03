module MIPS.Instruction.Format where
import Clash.Prelude
data Format = 
      RType (BitVector 6) (BitVector 5) (BitVector 5) (BitVector 5) (BitVector 5) (BitVector 6)
    | IType (BitVector 6) (BitVector 5) (BitVector 5) (BitVector 16)
    deriving Show

decodeFormat :: BitVector 32 -> Format
decodeFormat vec = 
    let opcode = slice d31 d26 vec in
    if opcode == 0
    then pure RType <*> (slice d31 d26) <*> (slice d25 d21) <*> (slice d20 d16) <*> (slice d15 d11) <*> (slice d10 d6) <*> (slice d5 d0) $ vec
    else pure IType <*> (slice d31 d26) <*> (slice d25 d21) <*> (slice d20 d16) <*> (slice d15 d0) $ vec