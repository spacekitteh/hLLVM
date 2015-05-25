#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -eq 0 ]; then
    echo "usage: $0 <the c file> [include or link options]"
    exit 1;
fi



bname=`basename $1 .c`
dname=`dirname $1`
first_argument=$1
shift
other_arguments="$@"

echocolortext ${yellow} "Compiling code with dynamic type safety checks"

runCmd "rm -f $bname.bc"
runCmd "$CLANG -g -emit-llvm -c $first_argument -o $bname.bc -I$RT_INCLUDE_PATH"
runCmd "$LLVM_DIS $bname.bc -o $bname.ll"
runCmd "$OPT -O1 -S $bname.ll -o $bname.O1.ll"
runCmd "rm -f $bname.hir.ll"
sizeofCheck $bname
runCmd "$LLVM_TEST_CMD ir2ast --input $bname.ll --output $bname.hir.ll"
runCmd "${SCRIPT_PATH}/strip.sh $bname.hir.ll"
runCmd "$OPT -O1 -S $bname.hir.ll -o $bname.hir.O1.ll"
runCmd "$LLC $bname.hir.ll -o $bname.hir.s"
runCmd "$CLANG $bname.hir.s"
