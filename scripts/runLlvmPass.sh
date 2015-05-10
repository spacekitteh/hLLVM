#!/bin/bash
SCRIPT_PATH=$(dirname $(readlink -f $0))
. $SCRIPT_PATH/enviroment.sh

if [ "$#" -ne 2 ]; then
   echo "usage: $0 <step> <absolute_path_of_llvm_test_directory>"
   exit 1;
fi

echo "search $2 for llvm files..."
find $2 -name "*.ll" > /tmp/llvmfiles.list

if [ -f skipped.list ]; then
    rm skipped.list
fi

for i in $(cat /tmp/llvmfiles.list); do
    $LLVM_AS $i 2>/dev/null
    if [ "$?" -eq 0 ]; then
	echocolortext ${cyan} "test $1 on $i"
	$LLVM_TEST_CMD $1 -i $i -o /tmp/llvm-test-parser.ll
	if [ "$?" -ne 0 ]; then
	    exitWithError "failed to $1 $i"
	fi
    else
        echocolortext ${red} "${LLVM_AS} failed to parse or verfiy $i, skip it."
        echo $i >> skipped.list
    fi
done

echocolortext ${red} "`cat skipped.list | wc -l` files are skipped!"
echocolortext ${green} "All tests are done!"
