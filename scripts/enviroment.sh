#!/bin/bash

THIS_FILE_PATH=$(dirname $(readlink -f $0))
bold=`tput -T xterm bold`
red=`tput -T xterm setaf 1`
green=`tput -T xterm setaf 2`
yellow=`tput -T xterm setaf 3`
blue=`tput -T xterm setaf 4`
magenta=`tput -T xterm setaf 5`
cyan=`tput -T xterm setaf 6`
white=`tput -T xterm setaf 7`
smul=`tput -T xterm smul`
rmul=`tput -T xterm rmul`

reset=`tput -T xterm sgr0`

CLANG=clang
LLVM_DIS=llvm-dis
LLVM_AS=llvm-as
LLC=llc
OPT=opt
GCC=gcc
LLVM_LINK=llvm-link

# set up the environment for all scripts in this folder
if [ "$LLVM_PATH" == "" ]; then
    echocolortext ${yellow} "LLVM_PATH is not set"
    echocolortext ${yellow} "Using the default path of LLVM"
else
    CLANG=$LLVM_PATH/$CLANG
    LLVM_DIS=$LLVM_PATH/$LLVM_DIS
    LLVM_AS=$LLVM_PATH/$LLVM_AS
    LLC=$LLVM_PATH/$LLC
    OPT=$LLVM_PATH/$OPT
    LLVM_LINK=$LLVM_PATH/$LLVM_LINK
fi

BASE_PATH=$THIS_FILE_PATH/..

LLVM_TEST_CMD=$BASE_PATH/dist/build/llvm-test/llvm-test

RT_LIB_PATH=$BASE_PATH/runtime
RT_INCLUDE_PATH=$BASE_PATH/runtime

##
## echocolortext ${red} "... text "
##
echocolortext () {
    echo "${1}${2}${reset}"
}

exitWithError () {
    echo "${red}${1}${reset}"
    exit 1;
}


##
## runCmd "ls"
##
runCmd ()
{
    cmd=$1
    echocolortext ${cyan} "running: $cmd"
    $cmd
    if [ "$?" -ne 0 ]; then
	exitWithError "failed to run:${cmd}"
    fi
}
