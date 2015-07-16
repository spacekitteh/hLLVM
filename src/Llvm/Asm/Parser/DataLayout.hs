module Llvm.Asm.Parser.DataLayout where

import Llvm.Asm.Parser.Basic
import Llvm.Asm.Data.DataLayout
import Llvm.Asm.Data.AtomicEntity (TargetTriple(..), Arch(..), Vendor(..), Os(..), OsEnv(..))


pLayoutSpec :: P LayoutSpec
pLayoutSpec = choice [ char 'e' >> return (DlE LittleEndian)
                     , char 'E' >> return (DlE BigEndian)
                     , try (char 's' >> do { n <- option Nothing (liftM Just unsignedInt)
                                           ; aa <- option Nothing (colon >> liftM Just unsignedInt)
                                           ; pa <- option Nothing (colon >> liftM Just unsignedInt)
                                           ; return (DlLittleS n aa pa)
                                           })
                     , char 'p' >> do { as <- option (LayoutAddrSpaceUnspecified) (liftM LayoutAddrSpace unsignedInt)
                                      ; s <- colon >> liftM (SizeInBit . fromIntegral) unsignedInt
                                      ; am <- colon >> alignMetrics
                                      ; return (DlP as s am)
                                      }
                     , dlIVF
                     , char 'a' >> do { am <- colon >> alignMetrics
                                      ; return (DlA am)
                                      }
                     , char 'm' >> colon >> liftM DlM (choice [ char 'e' >> return ManglingE
                                                              , char 'm' >> return ManglingM
                                                              , char 'o' >> return ManglingO
                                                              , char 'w' >> return ManglingW
                                                              ])
                     , char 'n' >> do { ls <- sepBy1 unsignedInt colon
                                      ; return (DlN (fmap (SizeInBit . fromIntegral) ls))
                                      }
                     , try (symbol "S0" >> return (DlS StackAlignUnspecified))
                     , char 'S' >> liftM ((DlS . StackAlign . AlignInBit . fromIntegral)) unsignedInt
                     ]
  where alignMetrics = do { aa <- liftM (AlignInBit . fromIntegral) unsignedInt
                          ; pa <- option Nothing (colon >> liftM (Just . AlignInBit . fromIntegral) unsignedInt)
                          ; return (AlignMetrics aa pa)
                          }
        dlIVF = do { df <- choice [ char 'i' >> return DlI
                                  , char 'v' >> return DlV
                                  , char 'f' >> return DlF
                                  ]
                   ; s <- liftM (SizeInBit . fromIntegral) unsignedInt
                   ; am <- colon >> alignMetrics
                   ; return (df s am)
                   }
              
pDataLayout :: P DataLayout
pDataLayout = liftM DataLayout (sepBy pLayoutSpec (char '-'))


pTargetTriple :: P TargetTriple
pTargetTriple = 
  do { arch <- choice [ reserved "i386" >> return Arch_i386
                      , reserved "i686" >> return Arch_i686
                      , reserved "x86" >> return Arch_x86
                      , reserved "x86_64" >> return Arch_x86_64
                      , reserved "powerpc" >> return Arch_PowerPc
                      , reserved "powerpc64" >> return Arch_PowerPc64
                      , reserved "arm" >> liftM Arch_Arm pAnyString
                      , reserved "thumbv7" >> return Arch_ThumbV7
                      , reserved "mips" >> liftM Arch_Mips pAnyString
                      , reserved "itanium" >> return Arch_Itanium 
                      , liftM Arch_String pAnyString
                      ]
     ; vendor <- option Nothing (char '-' >> liftM Just
                                 (choice [ reserved "pc" >> return Vendor_Pc
                                         , reserved "apple" >> return Vendor_Apple
                                         , reserved "unknown" >> return Vendor_Unknown
                                         , liftM Vendor_String pAnyString
                                         ]))
     ; os <- option Nothing (char '-' >> liftM Just
                             (choice [ reserved "linux" >> return Os_Linux
                                     , reserved "windows" >> return Os_Windows
                                     , reserved "win32" >> return Os_Win32
                                     , reserved "mingw32" >> return Os_Mingw32
                                     , reserved "unknown" >> return Os_Unknown
                                     , reserved "darwin" >> liftM Os_Darwin pAnyString
                                     , string "freebsd" >> liftM Os_FreeBsd pAnyString
                                     , string "macosx" >> liftM Os_Macosx pAnyString
                                     , string "ios" >> liftM Os_Ios pAnyString
                                     , liftM Os_String pAnyString
                                     ]))
     ; env <- option Nothing (char '-' >> liftM Just (choice [ reserved "gnu" >> return OsEnv_Gnu
                                                             , liftM OsEnv_String pAnyString
                                                             ]))
     ; return $ TargetTriple arch vendor os env
     }
  where 
    pAnyString :: P String
    pAnyString = (manyTill anyChar (lookAhead $ choice [char '"', char '-']))
