module Llvm.AsmParser.DataLayout where

import Llvm.AsmParser.Basic
import Llvm.VmCore.DataLayout


pLayoutSpec :: P LayoutSpec
pLayoutSpec = choice [ char 'e' >> return (DlE LittleEndian)
                     , char 'E' >> return (DlE BigEndian)
                     , try (char 's' >> do { n <- option Nothing (liftM Just integer)
                                           ; aa <- option Nothing (colon >> liftM Just integer)
                                           ; pa <- option Nothing (colon >> liftM Just integer)
                                           ; return (DlLittleS n aa pa)
                                           })
                     , char 'p' >> do { as <- option (LayoutAddrSpaceUnspecified) (liftM LayoutAddrSpace integer)
                                      ; s <- colon >> liftM SizeInBit integer
                                      ; aa <- colon >> liftM (AbiAlign . AlignInBit) integer
                                      ; pa <- prefAlign
                                      ; return (DlP as s aa pa) 
                                      }
                     , dlIVF
                     , char 'a' >> do { s <- opt (liftM SizeInBit integer)
                                      ; aa <- colon >> liftM (AbiAlign . AlignInBit) integer
                                      ; pa <- prefAlign
                                      ; return (DlA s aa pa)
                                      }
                     , char 'm' >> colon >> liftM DlM (choice [ char 'e' >> return ManglingE
                                                              , char 'm' >> return ManglingM
                                                              , char 'o' >> return ManglingO
                                                              , char 'w' >> return ManglingW
                                                              ])
                     , char 'n' >> do { ls <- sepBy1 integer colon
                                      ; return (DlN (fmap SizeInBit ls))
                                      }
                     , try (symbol "S0" >> return (DlS StackAlignUnspecified))
                     , char 'S' >> liftM ((DlS . StackAlign . AlignInBit)) integer
                     ]
  where prefAlign = option Nothing (colon >> liftM (Just . PrefAlign . AlignInBit) integer)
        dlIVF = do { df <- choice [ char 'i' >> return DlI
                                  , char 'v' >> return DlV
                                  , char 'f' >> return DlF
                                  ]
                   ; s <- liftM SizeInBit integer
                   ; abi <- colon >> liftM (AbiAlign . AlignInBit) integer
                   ; pa <- prefAlign
                   ; return (df s abi pa)
                   }
              
pDataLayout :: P DataLayout              
pDataLayout = liftM DataLayout (sepBy pLayoutSpec (char '-'))