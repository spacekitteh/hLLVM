#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -ne 1 ]; then
  echo "usage: $0 <the absolute path of C test files>"
  exit 1;
fi



LL_GEN_SCRIPT=$SCRIPT_PATH/llcodegen.sh

echocolortext ${cyan} "find ${1} -name \"*.c\" > /tmp/cfiles.list "
find ${1} -name "*.c" > /tmp/cfiles.list

for i in $(cat /tmp/cfiles.list); do
    runCmd "$LL_GEN_SCRIPT $i"
done
