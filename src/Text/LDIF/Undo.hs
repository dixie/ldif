{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Text.LDIF.Undo (
        undoLDIF
)
where
import Data.Maybe
import Text.LDIF.Types
import Text.LDIF.Utils
import Text.LDIF.Printer
import Data.List (nub, foldl')
import qualified Data.ByteString.Char8 as BC

-- | Warning message when undo can not be calculated
type Warning = String

-- | Calculate undo LDIF
undoLDIF :: LDIF -> (LDIF,[[Warning]])
undoLDIF (LDIF v xs) = let (ys,w) = let zs = map undoRecord $ reverse xs
                                    in (map fst zs, map snd zs)
                       in (LDIF v ys, filter (not . null) w)

-- | Calculate undo Record with possible warnings
undoRecord :: LDIFRecord -> (LDIFRecord,[Warning])
undoRecord (ContentRecord dn vals) = (ChangeRecord dn ChangeDelete,[])
undoRecord (ChangeRecord  dn (ChangeAdd _))     = (ChangeRecord dn ChangeDelete,[])
undoRecord (ChangeRecord  dn ChangeDelete)      = (ChangeRecord dn (ChangeAdd []), [wrnO dn "delete"])
undoRecord (ChangeRecord  dn (ChangeModify xs)) = let (x, w) = let ys = map undoMod $ reverse xs
                                                               in (map fst ys, map snd ys)
                                                  in (ChangeRecord dn (ChangeModify x), catMaybes w)

-- | Calculate undo Modification with possible warning
undoMod :: Modify -> (Modify,Maybe Warning)
undoMod (ModAdd a v)     = (ModDelete a v, Nothing)
undoMod (ModDelete a [])  = (ModAdd a [], Just $ wrnA a "delete")
undoMod (ModDelete a zs)  = (ModAdd a zs, Nothing)
undoMod (ModReplace a v) = (ModReplace a [], Just $ wrnA a "replace")

wrnA a op = concat [show $ aName a, " ", op]

wrnO dn op = concat [show dn, " ", op]
