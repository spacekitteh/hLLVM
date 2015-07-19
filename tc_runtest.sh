#!/bin/bash
MYPATH=$(dirname $(readlink -f $0))
NJOBS=`nproc`

sh config.sh
cabal build

${MYPATH}/scripts/runLlvmTest.sh astcanonic ~/LLVM_3.5_codes
${MYPATH}/scripts/runLlvmTest.sh ir2ast ~/LLVM_3.5_codes
${MYPATH}/scripts/runLlvmTest.sh change ~/LLVM_3.5_codes
