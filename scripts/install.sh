#!/bin/bash
SCRIPT_HOME=$(dirname $(readlink -f $0))
source ${SCRIPT_HOME}/enviroment.sh

if [ "$#" -ne 1 ]; then
    echo "usage: $0 <the folder bin/clang is located>"
    exit 1;
fi

dest=$1

if [ ! -f ${dest}/bin/clang ]; then
    echo "Cannot find ${dest}/bin/clang."
    echo "${dest} is not a Clang/LLVM installation folder."
    exit 1;
fi

runCmd "cp -a ${SCRIPT_HOME}/../dist ${dest}"
runCmd "cp ${SCRIPT_HOME}/enviroment.sh ${dest}/bin"
runCmd "cp ${SCRIPT_HOME}/hirverify.sh ${dest}/bin"
runCmd "cp ${SCRIPT_HOME}/clang-emp-hirverify ${dest}/bin"
runCmd "cp ${SCRIPT_HOME}/clang++-emp-hirverify ${dest}/bin"
