module TEST();
    reg clk, reset, enable;
    initial 
        begin
            clk = 0;
            reset = 0;
            enable = 1;
        end
    
        always
            #2000 clk = !clk;


    CPU cpu(clk, reset, enable);
    
    

    initial begin
        $monitor("STALL: %b\nPC/4: %b\nINSTURCTION(DECODED) %b\nRS: %b\nRSV: %b\nRT: %b\nRTV: %b\nIMM: %b\nALU_RES: %b\nAM_FW_0: %b\nAM_FW_1: %b\nWDATA: %b\nRDATA: %b\n",  
        cpu.STALL, cpu.IM.PC_VALUE, cpu.PC_INSTRUCTION, cpu.DM_RS, cpu.DM_RSV, cpu.DM_RT, cpu.DM_RTV, cpu.DM_IMM, cpu.AM_RESULT, cpu.AM_FW_0, cpu.AM_FW_1, cpu.MM.MainMemory_res.EDIT_SERIAL, cpu.MM.MainMemory_res.DATA);
        #100000 $finish();
    end

endmodule