{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Text.LDIF.Undo (
        undoLDIF
)
where
import Prelude
import Text.LDIF.Types
import Text.LDIF.Utils
import Text.LDIF.Printer
import Data.List (nub, foldl')
import qualified Data.ByteString.Char8 as BC


-- | Calculate undo LDIF
undoLDIF :: LDIF -> LDIF
undoLDIF (LDIF v xs) = LDIF v (map undoRecord $ reverse xs)

-- | Calculate undo Record
undoRecord :: LDIFRecord -> LDIFRecord
undoRecord (ContentRecord dn vals) = ChangeRecord dn ChangeDelete
undoRecord (ChangeRecord  dn (ChangeAdd _))     = ChangeRecord dn ChangeDelete
undoRecord (ChangeRecord  dn ChangeDelete)      = ChangeRecord dn (ChangeAdd [])
undoRecord (ChangeRecord  dn (ChangeModify xs)) = ChangeRecord dn (ChangeModify $ map undoMod $ reverse xs)
                                                 
-- | Calculate undo Modification
undoMod :: Modify -> Modify
undoMod (ModAdd a v)     = (ModDelete a v)
undoMod (ModDelete a v)  = (ModAdd a v)
undoMod (ModReplace a v) = (ModReplace a [])

