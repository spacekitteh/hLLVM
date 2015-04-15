#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -ne 1 ]; then
  echo "usage: $0 <file>"
  exit 1;
fi

bname=`basename $1 .c`

echocolortext ${white} "Generating a LLVM assembly file from a C file"

runCmd "$CLANG -g -emit-llvm -c $1 -o $bname.g.bc -I$RT_INCLUDE_PATH"
runCmd "$LLVM_DIS $bname.g.bc -o $bname.g.ll"

runCmd "$CLANG -emit-llvm -c $1 -o $bname.bc -I$RT_INCLUDE_PATH"
runCmd "$OPT -O1 $bname.bc -o $bname.O1.bc"
runCmd "$OPT -O2 $bname.bc -o $bname.O2.bc"
runCmd "$OPT -O3 $bname.bc -o $bname.O3.bc"

runCmd "$LLVM_DIS $bname.bc -o $bname.ll"
runCmd "$LLVM_DIS $bname.O1.bc -o $bname.O1.ll"
runCmd "$LLVM_DIS $bname.O2.bc -o $bname.O2.ll"
runCmd "$LLVM_DIS $bname.O3.bc -o $bname.O3.ll"
