module MIPS.Utils.Arithmetic where

import Clash.Prelude

addOverflow :: BitVector 32 -> BitVector 32 -> (BitVector 32, Bool)
addOverflow a b =
  let c = a + b
   in (c, (a ! 31) == (b ! 31) && (a ! 31) /= (c ! 31))

subOverflow :: BitVector 32 -> BitVector 32 -> (BitVector 32, Bool)
subOverflow a b =
  let c = a - b
   in (c, (a ! 31) /= (b ! 31) && (c ! 31) == (b ! 31))
