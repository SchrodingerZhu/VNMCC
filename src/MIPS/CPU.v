`timescale 100fs/100fs
module CPU 
    ( input  CLOCK // clock
    , input  RESET // reset
    , input  ENABLE
    );
wire [32:0] BRANCH;
wire [30:0] PC_INSTRUCTION;
wire [31:0] PC_VALUE;
wire [37:0] WRITE_PAIR;
wire STALL;
wire [5:0]  DM_WRITE;
wire [1:0]  DM_MEM;
wire [33:0] DM_BRANCH_FLAG;
wire [4:0]  DM_ALU;
wire [32:0] DM_IMM;
wire [4:0]  DM_RS;
wire [31:0] DM_RSV;
wire [4:0]  DM_RT;
wire [31:0] DM_RTV;
wire [31:0] DM_COUNTER;
wire [37:0] AM_FW_0;
wire [37:0] AM_FW_1;
wire [5:0]  AM_WRITE_REG;
wire [33:0] AM_MEM_OP;
wire [31:0] AM_RESULT;
wire [32:0] AM_BRANCH_TARGET;
wire [32:0] MMO_BRANCH;
wire [37:0] MMO_WRITE_PAIR;
wire [37:0] MMO_FORWARD;
wire [32:0] WB_BRANCH;
wire [37:0] WB_WRITE_PAIR;

    InstructionModule IM
    ( // Inputs
      .CLOCK(CLOCK), // clock
      .RESET(RESET), // reset
      .ENABLE(ENABLE),
      .BRANCH(BRANCH),
      .STALL(STALL),

      // Outputs
      .PC_INSTRUCTION(PC_INSTRUCTION),
      .PC_VALUE(PC_VALUE)
    );



    DecodeModule DM
    ( // Inputs
      .CLOCK(CLOCK) // clock
    , .RESET(RESET) // reset
    , .ENABLE(ENABLE)
    , .WRITE_REG(WRITE_PAIR)
    , .STALL(STALL)
    , .INSTRUCTION(PC_INSTRUCTION)
    , .COUNTER(PC_VALUE)

      // Outputs
    , .DM_WRITE(DM_WRITE)
    , .DM_MEM(DM_MEM)
    , .DM_BRANCH_FLAG(DM_BRANCH_FLAG)
    , .DM_ALU(DM_ALU)
    , .DM_IMM(DM_IMM)
    , .DM_RS(DM_RS)
    , .DM_RSV(DM_RSV)
    , .DM_RT(DM_RT)
    , .DM_RTV(DM_RTV)
    , .DM_COUNTER(DM_COUNTER)
    );



    AithmeticModule AM
    ( // Inputs
      .CLOCK(CLOCK) // clock
    , .RESET(RESET) // reset
    , .ENABLE(ENABLE)
    , .FW_0(AM_FW_0)
    , .FW_1(AM_FW_1)
    , .STALL(STALL)
    , .AM_WRITE(DM_WRITE)
    , .AM_MEM(DM_MEM)
    , .AM_BRANCH_FLAG(DM_BRANCH_FLAG)
    , .AM_ALU(DM_ALU)
    , .AM_IMM(DM_IMM)
    , .AM_RS(DM_RS)
    , .AM_RSV(DM_RSV)
    , .AM_RT(DM_RT)
    , .AM_RTV(DM_RTV)
    , .AM_COUNTER(DM_COUNTER)

      // Outputs
    , .AM_WRITE_REG(AM_WRITE_REG)
    , .AM_MEM_OP(AM_MEM_OP)
    , .AM_RESULT(AM_RESULT)
    , .AM_BRANCH_TARGET(AM_BRANCH_TARGET)
    );


    MemoryModule MM
    ( // Inputs
      .CLOCK(CLOCK) // clock
    , .RESET(RESET) // reset
    , .ENABLE(ENABLE)
    , .MMI_WRITE_REG(AM_WRITE_REG)
    , .MMI_MEM_OP(AM_MEM_OP)
    , .MMI_RESULT(AM_RESULT)
    , .MMI_BRANCH_TARGET(AM_BRANCH_TARGET)
    , .STALL(STALL)

      // Outputs
    , .MMO_BRANCH(MMO_BRANCH)
    , .MMO_WRITE_PAIR(MMO_WRITE_PAIR)
    );



    WriteBack WB
    ( // Inputs
      .CLOCK(CLOCK) // clock
    , .RESET(RESET) // reset
    , .ENABLE(ENABLE)
    , .BRANCH(MMO_BRANCH)
    , .WRITE_PAIR(MMO_WRITE_PAIR)

      // Outputs
    , .WB_BRANCH(WB_BRANCH)
    , .WB_WRITE_PAIR(WB_WRITE_PAIR)
    );

    assign AM_FW_0 = MMO_WRITE_PAIR;

    assign AM_FW_1 = WB_WRITE_PAIR;

    assign WRITE_PAIR = WB_WRITE_PAIR;

    assign BRANCH  = WB_BRANCH;
    

    HazardUnit HU
    ( // Inputs
      .BRANCH(BRANCH)

      // Outputs
    , .STALL(STALL)
    );
    



endmodule