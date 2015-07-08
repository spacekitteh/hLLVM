#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -ne 2 ]; then
    echo "usage: $0 <inputBitcodeFile> <outputAsmTextFile>"
    exit 1;
fi

sizeofCheck ()
{
    bname=$1
    dname=`dirname $1`
    $LLVM_TEST_CMD sizeofverification --input $bname.ll --output $bname.sizeof.ll
    $LLC $bname.sizeof.ll -o $bname.sizeof.s
    $GCC $bname.sizeof.s ${SCRIPT_PATH}/check_int.c -o $bname.sizeof
     
    if [ "$dname" == "." ]; then
	./${bname}.sizeof
    else 	    
        ${bname}.sizeof
    fi
    if [ "$?" -ne 0 ]; then
	echocolortext ${red} "sizeof check failed!"
	exit 1;
    else
	rm $bname.sizeof
    fi
}


ifile=$1
ofile=$2
bname=`basename ${ifile} .bc`
dname=`dirname ${ifile}`
if [ "${dname}" == "./" ]; then 
  fname=${bname} 
else 
  fname=${dname}/${bname} 
fi

${LLVM_DIS} ${ifile} -o ${fname}.ll
sizeofCheck ${fname}
${LLVM_TEST_CMD} ir2ast --input ${fname}.ll --output ${fname}.dts.ll
mv ${fname}.dts.ll ${ofile}
