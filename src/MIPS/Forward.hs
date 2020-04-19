module MIPS.Forward where
import Clash.Prelude
{-# LANGUAGE MultiWayIf #-}
forwardUnit :: HiddenClockResetEnable dom
    => Signal dom (Maybe (Unsigned 5, BitVector 32)) -- exec register
    -> Signal dom (Maybe (Unsigned 5, BitVector 32)) -- load register
    -> Signal dom (Unsigned 5)  -- rs
    -> Signal dom (Maybe (Unsigned 5)) -- rt
    -> Signal dom (Maybe (BitVector 32), Maybe (BitVector 32))
forwardUnit a b c d = forwarding <$> a <*> b <*> c <*> d
    where
        forwarding exec load rs rt = 
            let rs' = case (exec, load) of
                    (_, Just (no, res)) | no == rs -> Just res
                    (Just (no, res), _) | no == rs -> Just res
                    _                              -> Nothing
                rt' = case rt of
                    Nothing -> Nothing
                    Just no' -> case (exec, load) of
                        (_, Just (no, res)) | no == no' -> Just res
                        (Just (no, res), _) | no == no' -> Just res
                        _                               -> Nothing
                in (rs', rt')
        

            

        