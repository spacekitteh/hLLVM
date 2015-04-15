#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -ne 1 ]; then
    echo "usage: $0 <the llvm file>"
    exit 1;
fi

bname=`basename $1 .c`

echo $CLANG

runCmd "$LLVM_AS $1"
