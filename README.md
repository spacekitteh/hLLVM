hLLVM -- A Haskell Library for analyzing and transforming LLVM assembly codes


Goal: 
==========================================
- Provide functionalities for performaning analysis and transformation of LLVM codes in pure Haskell



Build:
==============
From hLLVM toplevel directory

  cabal configure

  cabal build

The test driver 'llvm-test' is generated at dist/build/llvm-test

Test:
==============
## test LLVM assembly parser
dist/build/llvm-test/llvm-test parse -i test/test1.ll -o out.ll

## test mem2reg pass
dist/build/llvm-test/llvm-test pass -s=mem2reg -f=10000 -i test/test1.ll -o out.ll


## test dce pass
dist/build/llvm-test/llvm-test pass -s=dce -f=10000 -i test/test1.ll -o out.ll


## test mem2reg and dce passes
dist/build/llvm-test/llvm-test pass -s=mem2reg -s=dce -f=1000 -i test/test1.ll -o out.ll


## run tests in batch (llvm-test needs to be available in the executable search paths)
test/runLlvmTest.sh [parse|ast2ir|ir2ast] <directory of llvm-3.5 test cases>
