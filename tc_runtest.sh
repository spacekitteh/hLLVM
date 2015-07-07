#!/bin/bash
MYPATH=$(dirname $(readlink -f $0))
LLVM_EMP=~/llvm_emp

sh config.sh
cabal build
git clone https://github.com/mlite/llvm_emp ${LLVM_EMP}
${MYPATH}/scripts/install.sh ${LLVM_EMP}

~/llvmsandbox/bin/lnt runtest nt --no-timestamp --sandbox ~/testsandbox --cc ${LLVM_EMP}/debian7_32/bin/clang-emp-hirverify --cxx ${LLVM_EMP}/debian7_32/bin/clang++-emp-hirverify --test-suite ~/test-suite-3.5.0.src
