module MIPS.ArithmeticModule where
import           Clash.Prelude
import           MIPS.ALU
import           MIPS.ControlUnit
import           MIPS.Forward
import           MIPS.Instruction.Type
import           MIPS.RegisterFile
import           Control.Monad.State
import           MIPS.Utils.State

data MemoryOperation' = MemNone'
    | MemLoad'
    | MemWrite' (BitVector 32)
    deriving Generic
    deriving NFDataX
    deriving Show


type ALUState =
  ( Maybe (Unsigned 5)                    -- write register
  , MemoryOperation                       -- memory
  , BranchFlag                            -- branch flag
  , ALUOperation                          -- ALU control
  , Maybe (BitVector 32)                  -- immediate value
  , RegNo                                 -- rs
  , BitVector 32                          -- rs value
  , RegNo                                 -- rd
  , BitVector 32                          -- rd value
  , Unsigned 32)                          -- counter

type ALUOutput =
    ( Maybe (Unsigned 5)                  -- write register
    , MemoryOperation'                    -- memory
    , BitVector 32                        -- ALU result
    , Maybe (Unsigned 32)                 -- branch target
    )

arithmeticModuleState' :: (ALUState, Bool) -> State ALUState ALUState
arithmeticModuleState' (_,       True) = do 
    let res = (Nothing, MemNone, NoBranch, ALUNone, Nothing, 0, 0, 0, 0, 0)
    put res
    return res

arithmeticModuleState' (state,  False) = do
    res <- get
    put state
    return res


arithmeticModuleState :: HiddenClockResetEnable dom => Signal dom (ALUState, Bool) ->  Signal dom ALUState
arithmeticModuleState = asStateM arithmeticModuleState' (Nothing, MemNone, NoBranch, ALUNone, Nothing, 0, 0, 0, 0, 0)


{-# ANN arithmeticModule (Synthesize {
    t_name = "AithmeticModule",
    t_inputs = [PortName "CLOCK",
        PortName "RESET",
        PortName "ENABLE",
        PortName "FW_0",
        PortName "FW_1",
        PortName "STALL",
        PortProduct "AM" [
           PortName "WRITE",
           PortName "MEM",
           PortName "BRANCH_FLAG",
           PortName "ALU",
           PortName "IMM",
           PortName "RS",
           PortName "RSV",
           PortName "RT",
           PortName "RTV",
           PortName "COUNTER"
        ]
       ],
    t_output = PortProduct "AM" [PortName "WRITE_REG", PortName "MEM_OP", PortName "RESULT", PortName "BRANCH_TARGET"]
}) #-}
arithmeticModule :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System ForwardInfo -- last
    -> Signal System ForwardInfo -- last last
    -> Signal System Bool
    -> Signal System ALUState           -- input
    -> Signal System ALUOutput           -- output
arithmeticModule clk rst enable last last' stall input =
    let
        (write, mem, branch, alu, imm, rs, rsv, rt, rtv, counter) = 
            unbundle $ ((exposeClockResetEnable arithmeticModuleState) clk rst enable) $ bundle (input, stall)
        (check0, check1) = 
            unbundle $ forwardUnit clk rst enable last last' rs rt

        unwrap (Just a) = a
        unwrap       _  = 0
        
        rsv' =  (<|>) <$> check0 <*> (pure <$> rsv)
        rtv0 =  ((<|>) <$> check1 <*> (pure <$> rtv))
        rtv' =  (<|>) <$> imm <*> rtv0

        memSolver MemWrite value = MemWrite' value
        memSolver MemLoad   _    = MemLoad'
        memSolver _         _    = MemNone'
        mem' = memSolver <$> mem <*> (unwrap <$> rtv0)

        (res, _, z, _) = unbundle $ arithmeticUnit <$> alu <*> (unwrap <$> rsv') <*> (unwrap <$>rtv')
        check_branch True  (BranchEQ delta) pc _   = Just (pc + unpack delta)
        check_branch False (BranchNE delta) pc _   = Just (pc + unpack delta)
        check_branch _     Jump  _    (Just i)     = Just (unpack i)
        check_branch _     _     _      _   = Nothing
        branch' = check_branch <$> z <*> branch <*> counter <*> imm
    in bundle (write, mem', res, branch')
