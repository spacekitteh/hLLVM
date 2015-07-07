#!/bin/bash
MYPATH=$(dirname $(readlink -f $0))

cabal build
git clone https://github.com/mlite/llvm_emp ~/llvm_emp
${MYPATH}/scripts/install.sh ~/llvm_emp

