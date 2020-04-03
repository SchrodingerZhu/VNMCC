`timescale 100fs/100fs
module MANUAL_TEST;
reg[31:0]   i_datain;
reg[31:0]   gr1;
reg[31:0]   gr2;

TEST test(
    .instr(i_datain),
    .in0(gr1), 
    .in1(gr2)
);

initial
begin
$monitor("\ntime      %0tns\ninstr     %b\nregister0 %b\nregister1 %b\noutput    %b\nhigh      %b\nlow       %b\nzero      %b\noverflow  %b\nneg       %b\nreg0      %b\nreg1      %b\ntarget    %b\nbranch    %b"
    , $time
    , i_datain
    , gr1
    , gr2
    , test.TEST_CHIP_out
    , test.TEST_CHIP_HI
    , test.TEST_CHIP_LOW
    , test.TEST_CHIP_V
    , test.TEST_CHIP_Z
    , test.TEST_CHIP_N
    , test.TEST_CHIP_REG0
    , test.TEST_CHIP_REG1
    , test.TEST_CHIP_TARGET
    , test.TEST_CHIP_BRANCH
);

#1000 i_datain<=32'b0000_0000_0000_0001_0001_0000_0100_0000;
gr2<=32'b1101_1101_1101_1101_1101_1101_1101_1101;

#1000 i_datain<=32'b0000_0000_0000_0001_0001_0000_1000_0000;
gr2<=32'b1101_1101_1101_1101_1101_1101_1101_1101; 


#1000 i_datain<=32'b0000_0000_0000_0001_0001_0000_0100_0000;
gr2<=32'b0100_0000_0100_0000_0100_0000_0100_0000;

#1000 i_datain<=32'b0000_0000_0000_0001_0001_0001_0000_0000;
gr2<=32'b0100_0000_0100_0000_0100_0000_0100_0000;

#1000 i_datain<=32'b0001_0100_0100_0011_1111_1111_1111_1111;
gr1<=32'b0000_0000_0000_0000_0100_0000_0000_0001;
gr2<=32'b0000_0000_0000_0000_0000_0000_0000_0011;

#1000 $finish;

end
endmodule