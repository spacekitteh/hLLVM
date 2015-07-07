#!/bin/bash
MYPATH=$(dirname $(readlink -f $0))
source ${MYPATH}/enviroment.sh

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

runCmd "cp -a ${MYPATH}/../dist ${dest}"
runCmd "${MYPATH}/cleantix.sh"
runCmd "cp ${MYPATH}/enviroment.sh ${dest}/bin"
runCmd "cp ${MYPATH}/hirverify.sh ${dest}/bin"
runCmd "cp ${MYPATH}/clang-emp-hirverify ${dest}/bin"
runCmd "cp ${MYPATH}/clang++-emp-hirverify ${dest}/bin"
