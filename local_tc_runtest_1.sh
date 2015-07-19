#!/bin/bash
MYPATH=$(dirname $(readlink -f $0))
LLVM_EMP=~/llvm_emp
NJOBS=`nproc`

cabal clean
sh config.sh
cabal build

if [ -d ${LLVM_EMP} ]; then
	cd ${LLVM_EMP}
	git pull
else 
	git clone https://github.com/mlite/llvm_emp ${LLVM_EMP}
fi

${MYPATH}/scripts/install.sh ${LLVM_EMP}/debian7_32

rm -rf ~/single_test
mkdir ~/single_test
${LLVM_EMP}/debian7_32/bin/findandrun_goodc.sh ~/test_bench/lang_C/single_2
${MYPATH}/scripts/genhpcreport.sh llvm-test.tix ~/single_test
