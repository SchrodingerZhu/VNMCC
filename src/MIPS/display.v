module WATCH;
SEQ_TEST_testBench bench();
initial
begin
    $monitor("\ntime      %0tns\ninstr     %b\nregister0 %b\nregister1 %b\noutput    %b\nhigh      %b\nlow       %b\nzero      %b\noverflow  %b\nneg       %b\nreg0      %b\nreg1      %b\ntarget    %b\nbranch    %b"
    , $time
    , bench.instr
    , bench.in0
    , bench.in1
    , bench.TEST_CHIP_out
    , bench.TEST_CHIP_HI
    , bench.TEST_CHIP_LOW
    , bench.TEST_CHIP_V
    , bench.TEST_CHIP_Z
    , bench.TEST_CHIP_N
    , bench.TEST_CHIP_REG0
    , bench.TEST_CHIP_REG1
    , bench.TEST_CHIP_TARGET
    , bench.TEST_CHIP_BRANCH
);
end
endmodule
