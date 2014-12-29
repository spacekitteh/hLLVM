module Llvm.AsmParser.DataLayout where

import Llvm.AsmParser.Basic
import Llvm.VmCore.DataLayout


pLayoutSpec :: P LayoutSpec
pLayoutSpec = choice [ char 'e' >> return (DlE LittleEndian)
                     , char 'E' >> return (DlE BigEndian)
                     , char 'S' >> option (DlS StackAlignUnspecified) (liftM (DlS . StackAlign) integer) 
                     , char 'p' >> do { as <- option (LayoutAddrSpaceUnspecified) (liftM LayoutAddrSpace integer)
                                      ; s <- colon >> liftM Size integer
                                      ; aa <- colon >> liftM AbiAlign integer
                                      ; pa <- prefAlign
                                      ; return (DlP as s aa pa) 
                                      }
                     , dlIVF
                     , char 'a' >> do { s <- liftM Size integer
                                      ; aa <- colon >> liftM AbiAlign integer
                                      ; pa <- prefAlign
                                      ; return (DlA s aa pa)
                                      }
                     , char 'm' >> colon >> liftM DlM (choice [ char 'e' >> return ManglingE
                                                              , char 'm' >> return ManglingM
                                                              , char 'o' >> return ManglingO
                                                              , char 'w' >> return ManglingW
                                                              ])
                     , char 'n' >> do { s1 <- liftM Size integer
                                      ; s2 <- colon >> liftM Size integer
                                      ; s3 <- colon >> liftM Size integer
                                      ; return (DlN s1 s2 s3)
                                      }
                     ]
  where prefAlign = option Nothing (colon >> liftM (Just . PrefAlign) integer)
        dlIVF = do { df <- choice [ char 'i' >> return DlI
                                  , char 'v' >> return DlV
                                  , char 'f' >> return DlF
                                  ]
                   ; s <- liftM Size integer
                   ; abi <- colon >> liftM AbiAlign integer
                   ; pa <- prefAlign
                   ; return (df s abi pa)
                   }
              
pDataLayout :: P DataLayout              
pDataLayout = liftM DataLayout (sepBy pLayoutSpec (char '-'))