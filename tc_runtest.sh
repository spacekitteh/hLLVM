#!/bin/bash
MYPATH=$(dirname $(readlink -f $0))
LLVM_CMP=~/llvm_cmp

sh config.sh
cabal build
git clone https://github.com/mlite/llvm_emp ${LLVM_CMP}
${MYPATH}/scripts/install.sh ${LLVM_CMP}

~/llvmsandbox/bin/lnt runtest nt --no-timestamp --sandbox ~/testsandbox --cc ${LLVM_CMP}/debian7_32/bin/clang-emp-hirverify --cxx ${LLVM_CMP}/debian7_32/bin/clang++-emp-hirverify --test-suite ~/test-suite-3.5.0.src
