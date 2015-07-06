#!/bin/bash
SCRIPT_HOME=$(dirname $(readlink -f $0))
source ${SCRIPT_HOME}/enviroment.sh

if [ "$#" -ne 1 ]; then
    echo "usage: $0 <the folder bin/clang is located>"
    exit 1;
fi

runCmd "cp -a ${SCRIPT_HOME}/../dist $1"
runCmd "cp ${SCRIPT_HOME}/enviroment.sh $1/bin"
runCmd "cp ${SCRIPT_HOME}/hirverify.sh $1/bin"
runCmd "cp ${SCRIPT_HOME}/clang-emp-hirverify $1/bin"




