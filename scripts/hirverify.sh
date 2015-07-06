#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -ne 2 ]; then
    echo "usage: $0 <inputBitcodeFile> <outputAsmTextFile>"
    exit 1;
fi

ifile=$1
bname=`basename ${ifile} .bc`
dname=`dirname ${ifile}`
if [ "${dname}" == "./" ]; then 
  fname=${bname} 
else 
  fname=${dname}/${bname} 
fi

runCmd "${LLVM_DIS} ${ifile} -o ${fname}.ll"
runCmd "${LLVM_TEST_CMD} ir2ast --input ${fname}.ll --output ${fname}.dts.ll"

echocolortext ${yellow} "Saving ${fname}.dts.ll as ${fname}.bc"
runCmd "mv ${fname}.dts.ll $2"
