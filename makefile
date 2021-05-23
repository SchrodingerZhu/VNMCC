MAKEFLAGS += --silent

modules = \
	MIPS.HazardUnit \
	MIPS.ControlUnit \
	MIPS.RegisterFile \
	MIPS.RAM \
	MIPS.DecodeModule \
	MIPS.WriteBack \
	MIPS.Forward \
	MIPS.PC \
	MIPS.MemoryModule \
	MIPS.ArithmeticModule \
	MIPS.ALU

clash_src = $(wildcard src/MIPS/**/*.hs)
verilog_src = $(wildcard src/MIPS/**/*.v verilog/MIPS/**/*.v)

all: gen build

gen:
	echo [Gen] Generating MIPS files...
	echo -e ":verilog $(modules) \n :q" | stack repl --with-ghc clash

build:
	echo [Build] Building the project...
	iverilog -Wall -Winfloop $(verilog_src) -o MIPS_CPU
	echo [Build] The generated file is available at ./MIPS_CPU

run:
	echo [Run] Running...
	./MIPS_CPU

clean:
	echo [Clean] Cleaning...
	rm -rf verilog MIPS_CPU
	echo [Clean] Done
