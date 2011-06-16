{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | LDIF serializers
module Text.LDIF.Printer (
	ldif2str,
        dn2str,
	record2str
)
where
import Prelude as P
import Text.LDIF.Types
import Text.LDIF.Consts
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Data.List as L
import Data.Char
import Numeric (showHex)

-- | Serialize LDIF in LDIF Format
ldif2str :: LDIF -> ByteString
ldif2str (LDIF v xs) = BC.unlines $ (ver2str v) ++ (P.map (record2str) xs)

-- | Serialize version to LDIF Format Lines
ver2str :: Maybe ByteString -> [ByteString]
ver2str Nothing = []
ver2str (Just v) = ["version: " `B.append` v]

-- | Serialize DN to LDIF Format
dn2str :: DN -> ByteString
dn2str xs = BC.intercalate "," $ P.map (\((Attribute n),v) -> n `B.append` "="  `B.append` (escapeDNVals v)) (dnAttrVals xs)

escapeDNVals :: ByteString -> ByteString
escapeDNVals vs = B.concat $ P.map escapeDNVal (BC.unpack vs)
  where
    escapeDNVal x | not $ isPrint x          = BC.pack $ '\\':(showHex (ord x) "")
                  | P.elem x escapedDNChars  = BC.pack $ '\\':[x]
                  | otherwise                = BC.pack $ [x]

-- | Serialize Content Record in LDIF Format
record2str :: LDIFRecord -> ByteString
record2str (ContentRecord dn xs)                = BC.unlines $ [ "dn: " `B.append` (dn2str dn) ] ++ (attrVals2Ln xs)
record2str (ChangeRecord dn (ChangeDelete))     = BC.unlines   [ "dn: " `B.append` (dn2str dn), "changetype: delete" ]
record2str (ChangeRecord dn (ChangeAdd xs))     = BC.unlines $ [ "dn: " `B.append` (dn2str dn), "changetype: add"    ] ++ (attrVals2Ln xs)
record2str (ChangeRecord dn (ChangeModify xs))  = BC.unlines $ [ "dn: " `B.append` (dn2str dn), "changetype: modify" ] ++ (mods2Ln xs)
record2str (ChangeRecord dn (ChangeModDN))      = BC.unlines $ [ "dn: " `B.append` (dn2str dn), "changetype: moddn"  ]

attrVals2Ln :: [AttrValue] -> [ByteString]
attrVals2Ln xs = P.map (attrVal2Ln) xs

attrVal2Ln :: AttrValue -> ByteString
attrVal2Ln ((Attribute n),v) = B.concat [ n,": ",v ]

mods2Ln :: [Modify] -> [ByteString]
mods2Ln xs = L.intercalate ["-"] $ P.map (mod2Ln) xs

mod2Ln :: Modify -> [ByteString]
mod2Ln (ModAdd     a@(Attribute nm) xs) = [ attrVal2Ln ((Attribute "add"),nm)     ] ++ (P.map (\v -> attrVal2Ln (a,v)) xs) 
mod2Ln (ModDelete  a@(Attribute nm) xs) = [ attrVal2Ln ((Attribute "delete"),nm)  ] ++ (P.map (\v -> attrVal2Ln (a,v)) xs)
mod2Ln (ModReplace a@(Attribute nm) xs) = [ attrVal2Ln ((Attribute "replace"),nm) ] ++ (P.map (\v -> attrVal2Ln (a,v)) xs)
