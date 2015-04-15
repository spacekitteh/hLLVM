#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -ne 1 ]; then
    echo "usage: $0 <the llvm file>"
    exit 1;
fi



bname=`basename $1 .ll`

echocolortext ${yellow} "Stripping off comments in a LLVM file"

runCmd "$LLVM_AS $1 -o $bname.stripped.bc"
runCmd "$LLVM_DIS $bname.stripped.bc -o $bname.stripped.ll"
