#!/bin/bash
MYPATH=$(dirname $(readlink -f $0))
LLVM_EMP=~/llvm_emp
NJOBS=`nproc`

sh config.sh
cabal build
if [ -d ${LLVM_EMP} ]; then
	cd ${LLVM_EMP}
	git pull
else 
	git clone https://github.com/mlite/llvm_emp ${LLVM_EMP}
fi

${MYPATH}/scripts/install.sh ${LLVM_EMP}/debian7_32

rm -rf ~/testsandbox/build
~/llvmsandbox/bin/lnt runtest nt -j ${NJOBS} --no-timestamp --sandbox ~/testsandbox --cc ${LLVM_EMP}/debian7_32/bin/clang-emp-hirverify --cxx ${LLVM_EMP}/debian7_32/bin/clang++-emp-hirverify --test-suite ~/test-suite-3.5.0.src

${MYPATH}/scripts/genhpcreport.sh llvm-test.tix ~/testsandbox
