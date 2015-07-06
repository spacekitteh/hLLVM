#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

ifile=$1
bname=`basename ${ifile} .bc`
dname=`dirname ${ifile}`
if [ "${dname}" == "./" ]; then 
  fname=${bname} 
else 
  fname=${dname}/${bname} 
fi

runCmd "${LLVM_DIS} ${ifile} -o ${fname}.ll"
sizeofCheck ${fname}
runCmd "${LLVM_TEST_CMD} ir2ast --input ${fname}.ll --output ${fname}.dts.ll"

echocolortext ${yellow} "Saving ${fname}.dts.ll as ${fname}.bc"
runCmd "mv ${fname}.dts.ll -o $2"
