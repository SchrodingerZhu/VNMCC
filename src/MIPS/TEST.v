module TEST();
    reg clk, reset, enable;
    initial 
        begin
            clk = 0;
            reset = 0;
            enable = 1;
        end
    
        always
            #1000 clk = !clk;


    CPU cpu(clk, reset, enable);
    
    

    initial begin
        $monitor("=======================================================================\n",
                 "TIME:           %-d\n", $time, 
                 "STALL:          %b\n", cpu.STALL,
                 "-------------------------- Instruction --------------------------------\n",
                 "PC/4 + 1:       %-d\n", cpu.IM.PC_VALUE,
                 "INSTRUCTION:    %b\n", cpu.IM.result_0,
                 "INSTRUCTION:    %b [inner form]\n", cpu.IM.PC_INSTRUCTION,
                 "---------------------------- Decode -----------------------------------\n",
                 "RS:             %-d\n", cpu.DM_RS,
                 "RS VALUE:       %b\n", cpu.DM_RSV,
                 "RT:             %-d\n", cpu.DM_RT,
                 "RT VALUE:       %b\n", cpu.DM_RTV,
                 "MEM_OP:         %b\n", cpu.DM_MEM,
                 "REG_WRITE:      %b\n", cpu.DM_WRITE,
                 "ALU_CTL:        %b\n", cpu.DM_ALU,
                 "IMMEDIATE:      %b\n", cpu.DM_IMM,
                 "STAGE_PC/4 + 1: %-d\n", cpu.DM_COUNTER,
                 "--------------------------- Arithmetic --------------------------------\n",
                 "REG_WRITE:      %b\n", cpu.AM_WRITE_REG,
                 "MEM_OP:         %b\n", cpu.AM_MEM_OP,
                 "ALU_RESULT:     %b\n", cpu.AM_RESULT,
                 "BRANCH_TARGET:  %b\n", cpu.AM_BRANCH_TARGET,
                 "----------------------------- Memory ----------------------------------\n",
                 "BRANCH_TARGET:  %b\n", cpu.MMO_BRANCH,
                 "WRITE_BACK:     %b\n", cpu.MMO_WRITE_PAIR,
                 "FETCH_ADDRESS:  %-x\n", cpu.MM.MainMemory_res.FETCH_ADDRESS,
                 "FETCH_RESULT:   %-x\n", cpu.MM.MainMemory_res.DATA,
                 "WRITE_SERIAL:   %b\n",  cpu.MM.MainMemory_res.EDIT_SERIAL,
                 "--------------------------- Write Back --------------------------------\n",
                 "BRANCH_TARGET:  %b\n", cpu.WB_BRANCH,
                 "WRITE_BACK:     %b\n", cpu.WB_WRITE_PAIR,
                 "=======================================================================\n"
                 );
        #100000 $finish();
    end

endmodule