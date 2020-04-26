{-# LANGUAGE MultiWayIf #-}

module MIPS.Forward where

import Clash.Prelude

type ForwardInfo = Maybe (Unsigned 5, BitVector 32)

forwardUnit' ::
     HiddenClockResetEnable dom
  => Signal dom ForwardInfo -- mem register
  -> Signal dom ForwardInfo -- write-back register
  -> Signal dom (Unsigned 5) -- rs
  -> Signal dom (Unsigned 5) -- rt
  -> Signal dom (Maybe (BitVector 32), Maybe (BitVector 32))
forwardUnit' a b c d = forwarding <$> a <*> b <*> c <*> d
  where
    forwarding exec load rs rt =
      let rs' =
            case (exec, load) of
              (Just (no, res), _)
                | no == rs -> Just res
              (_, Just (no, res))
                | no == rs -> Just res
              _ -> Nothing
          rt' =
            case (exec, load) of
              (Just (no, res), _)
                | no == rt -> Just res
              (_, Just (no, res))
                | no == rt -> Just res
              _ -> Nothing
       in (rs', rt')

{-# ANN forwardUnit
          (Synthesize{t_name = "ForwardUnit",
                      t_inputs =
                        [PortName "CLOCK", PortName "RESET", PortName "ENABLE",
                         PortName "FORWARD_A", PortName "FORWARD_B", PortName "RS",
                         PortName "RT"],
                      t_output =
                        PortProduct "FW" [PortName "OVERRIDE_RS", PortName "OVERRIDE_RT"]})
        #-}

forwardUnit ::
     Clock System
  -> Reset System
  -> Enable System
  -> Signal System ForwardInfo -- exec register
  -> Signal System ForwardInfo -- load register
  -> Signal System (Unsigned 5) -- rs
  -> Signal System (Unsigned 5) -- rt
  -> Signal System (Maybe (BitVector 32), Maybe (BitVector 32))
forwardUnit = exposeClockResetEnable forwardUnit'
