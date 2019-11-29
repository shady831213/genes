#!/bin/bash
#compile tests
make -C riscv-tests/isa        RISCV_PREFIX=riscv32-unknown-elf- XLEN=32
make -C riscv-tests/isa        RISCV_PREFIX=riscv64-unknown-elf- XLEN=64

cd src/test/resources/scripts/elf2hex
#gen rv32* hex files
for i in `find ../../../../../riscv-tests/isa/rv32*-*`;
do
if [ ${i##*.} != "dump" ]
then
./elf2hex.sh --isa riscv32 --bit-width 32 --input $i --output $i.hex
fi
done

#gen rv64* hex files
for i in `find ../../../../../riscv-tests/isa/rv64*-*`;
do
if [ ${i##*.} != "dump" ]
then
./elf2hex.sh --isa riscv64 --bit-width 32 --input $i --output $i.hex
fi
done

cd ../../../../..
mv riscv-tests/isa/*.hex src/test/resources/hex
