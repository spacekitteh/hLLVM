#!/bin/bash
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

if [ "$#" -ne 2 ]; then
   echo "usage: $0 <pass> <absolute_path_of_llvm_test_directory>"
   exit 1;
fi

echo "search $2 for llvm files..."
find $2 -name "*.ll" > /tmp/llvmfiles.list

if [ -f skipped.list ]; then
   rm skipped.list
fi

for i in $(cat /tmp/llvmfiles.list); do
    llvm-as $i 2>/dev/null
    if [ "$?" -eq 0 ]; then
	echo "test $1 on $i"
	llvm-test $1 --i $i -o /tmp/llvm-test-parser.ll
	if [ "$?" -ne 0 ]; then
	    echo "failed to parse $i"
	    exit 1;
	fi
	llvm-as /tmp/llvm-test-parser.ll
	if [ "$?" -ne 0 ]; then
	    echo "failed to assemble the output of parsing $i"
	    echo "failed to assemble the output: /tmp/llvm-test-parser.ll"
	    exit 1;
	fi
    else
        echo "${red}llvm-as failed to parse or verfiy $i, skip it.${reset}"
        echo $i >> skipped.list
    fi
done
echo "${green}All tests are done!${reset}"
