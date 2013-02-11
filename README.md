HsLlvm -- Haskell implementation of LLVM


Goal: 
==========================================
Provide type safe manipulation of LLVM code in Haskell


Build:
==============
From LLVM toplevel directory

  cabal configure
  cabal build

The test driver 'llvm-test' is generated at dist/build/llvm-test

Test:
==============
## test LLVM asmebler parser
dist/build/llvm-test/llvm-test parser test/test1.ll 

## test mem2reg pass
dist/build/llvm-test/llvm-test pass -s=mem2reg -f=10000 test/test1.ll


## test dce pass
dist/build/llvm-test/llvm-test pass -s=dce -f=10000 test/test1.ll


## test mem2reg and dce passes
dist/build/llvm-test/llvm-test pass -s=mem2reg -s=dce -f=1000 test/test1.ll 
