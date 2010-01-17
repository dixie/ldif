-- | Make Change LDIF based on two Content LDIFs
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF

ldif2html :: LDIF -> String
ldif2html (LDIFContent v xs) = unlines $ (ver2Str v) ++ (map (content2html) xs)
ldif2html (LDIFChanges v xs) = unlines $ (ver2Str v) ++ (map (change2html) xs)

-- | Serialize version to LDIF Format Lines
ver2Str :: Maybe String -> [String]
ver2Str Nothing = []
ver2Str (Just v) = ["version: "++v]

-- | Serialize DN to LDIF Format
dn2html :: DN -> String
dn2html (DN xs) = concat [ "<div id=\"abc"++nm++"\"></div><b>dn:</b> <font color=\"green\"><b>", intercalate "," $ map (\(n,v) -> n++"="++v) xs, "</b></font>"]
      where
         nm = snd $ head xs

-- | Serialize Content Record in LDIF Format
content2html :: ContentRecord -> String
content2html (ContentRecord dn xs) = unlines $ [ (dn2html dn) ] ++ (attrVals2Ln xs)

-- | Serialize Change Record in LDIF Format
change2html :: ChangeRecord -> String
change2html (ChangeRecord dn (ChangeDelete))     = unlines   [ (dn2html dn), "changetype: delete" ]
change2html (ChangeRecord dn (ChangeAdd xs))     = unlines $ [ (dn2html dn), "changetype: add"    ] ++ (attrVals2Ln xs)
change2html (ChangeRecord dn (ChangeModify xs))  = unlines $ [ (dn2html dn), "changetype: modify" ] ++ (mods2Ln xs)
change2html (ChangeRecord dn (ChangeModDN))      = unlines $ [ (dn2html dn), "changetype: moddn"  ]

attrVals2Ln :: [AttrValue] -> [String]
attrVals2Ln xs = map (attrVal2Ln) xs

attrVal2Ln :: AttrValue -> String
attrVal2Ln (n,v) = "<b>"++n++"</b> : <a href=\"#abc"++v++"\">"++v++"</a>"

mods2Ln :: [Modify] -> [String]
mods2Ln xs = intercalate ["-"] $ map (mod2Ln) xs

mod2Ln :: Modify -> [String]
mod2Ln (ModAdd     nm xs) = [ attrVal2Ln ("add",nm)     ] ++ (map (\v -> attrVal2Ln (nm,v)) xs) 
mod2Ln (ModDelete  nm xs) = [ attrVal2Ln ("delete",nm)  ] ++ (map (\v -> attrVal2Ln (nm,v)) xs)
mod2Ln (ModReplace nm xs) = [ attrVal2Ln ("replace",nm) ] ++ (map (\v -> attrVal2Ln (nm,v)) xs)

-- Dummy ldif2html implementation but it should
-- display LDIF file as HTML with values as href to
-- DN which has the value as the leaf RDN value
main = do
  args <- getArgs
  ml <- parseLDIFFile (args !! 0)
  case ml of 
       Right l -> putStrLn $ concat ["<html><body bgcolor=lightyellow><pre>", ldif2html $ l, "</pre></body></html>" ]
       Left e  -> print e
