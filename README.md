hLLVM -- A Haskell Library for analyzing and transforming LLVM assembly codes


Goal: 
==========================================
- Provide functionalities for performaning analysis and transformation of LLVM codes in pure Haskell

The implemented functionalities:
1) a parser to parse LLVM code in its text form; 
2) an internal IR that is designed for Hoopl and direct composition; 
3) a set of utility functions to convert LLVM AST to and from the internal IR; 
4) a set of utility functions to query the IR.


Build:
==============
From hLLVM toplevel directory

  cabal configure

  cabal build

The test driver 'llvm-test' is generated at dist/build/llvm-test

What work?
==============
From hLLVM toplevel directory

## get LLVM 3.5 test codes
./scripts/clone_LLVM_codes.sh

## test LLVM assembly parser
./dist/build/llvm-test/llvm-test parse -i LLVM_3.5_codes/&lt;any.ll&gt; -o out.ll

## test LLVM AST to IR conversion
./dist/build/llvm-test/llvm-test ast2ir -i LLVM_3.5_codes/<any.ll> -o out.ll

## test IR to LLVM AST conversion
./dist/build/llvm-test/llvm-test ir2ast -i LLVM_3.5_codes/<any.ll> -o out.ll

## run tests in batch 
./scripts/runLlvmTest.sh parse LLVM_3.5_codes

./scripts/runLlvmTest.sh ir2ast LLVM_3.5_codes


What don't work?
==============
The following functions broke after the refactory, I will fix ASAP.
## test mem2reg pass
dist/build/llvm-test/llvm-test pass -s=mem2reg -f=10000 -i test/test1.ll -o out.ll


## test dce pass
dist/build/llvm-test/llvm-test pass -s=dce -f=10000 -i test/test1.ll -o out.ll


## test mem2reg and dce passes
dist/build/llvm-test/llvm-test pass -s=mem2reg -s=dce -f=1000 -i test/test1.ll -o out.ll


