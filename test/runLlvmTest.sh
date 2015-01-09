#!/bin/bash

if [ "$#" -ne 1 ]; then
   echo "usage: $0 <absolute_path_of_llvm_test_directory>"
   exit 1;
fi

echo "search $1 for llvm files..."
find $1 -name "*.ll" > /tmp/llvmfiles.list

for i in $(cat /tmp/llvmfiles.list); do
    llvm-as $i
    if [ "$?" -eq 0 ]; then
	echo "test parser on $i"
	llvm-test parser $i -o /tmp/llvm-test-parser.ll
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
    fi
done
