HsLlvm -- Haskell implementation of LLVM


Goal: 
==========================================
- Port all LLVM optimizations
- Provide functionalities to generate and maniuplate LLVM code


Build:
==============
From HsLlvm toplevel directory

  cabal configure

  cabal build

The test driver 'llvm-test' is generated at dist/build/llvm-test

Test:
==============
## test LLVM assembly parser
dist/build/llvm-test/llvm-test parser --input test/test1.ll 

## test mem2reg pass
dist/build/llvm-test/llvm-test pass -s=mem2reg -f=10000 --input test/test1.ll


## test dce pass
dist/build/llvm-test/llvm-test pass -s=dce -f=10000 --input test/test1.ll


## test mem2reg and dce passes
dist/build/llvm-test/llvm-test pass -s=mem2reg -s=dce -f=1000 --input test/test1.ll 
