LLVM -- Haskell implementation of LLVM


Goal: 
==========================================
Provide type safe manipulation of LLVM code


Build:
==============
From LLVM toplevel directory

  cabal configure
  cabal build

The test driver 'llvm-test' is generated at dist/build/llvm-test

Test:
==============
## test LLVM asmebler parser
llvm-test parser test.ll 

## test mem2reg pass
llvm-test pass -s=mem2reg -f=10000 test/test.ll


## test dce pass
llvm-test pass -s=dce -f=10000 test/test.ll


## test multiple passes
llvm-test pass -s=mem2reg -s=dce -f=10000 test/test.ll
