module Text.LDIF.Apply (
        applyLDIF
)
where
import Text.LDIF.Types
import Text.LDIF.Utils
import Data.List (nub)

-- | Apply one LDIF to another LDIF. The destination LDIF has
-- | to be Content LDIF
applyLDIF :: LDIF -> LDIF -> LDIF
applyLDIF dst@(LDIFContent _ _) (LDIFChanges _ xs) = foldr (applyRecord2LDIF) dst xs
applyLDIF dst@(LDIFContent _ _) (LDIFContent _ xs) = foldr (applyRecord2LDIF) dst xs
applyLDIF _ _ = error "Destination LDIF has to be Content LDIF and not Change LDIF"

-- | Apply one LDIF Content/Change Record into LDIF and produce Changed LDIF
applyRecord2LDIF :: LDIFRecord -> LDIF -> LDIF
applyRecord2LDIF (ContentRecord dn vals) dst = applyRecord2LDIF (ChangeRecord dn (ChangeAdd vals)) dst
applyRecord2LDIF (ChangeRecord  dn op)   dst = applyChange2Record op dn dst (findRecordByDN dst dn)

-- | Apply one LDIF Change (add/del/modf) for given DN within LDIF Content 
applyChange2Record :: Change -> DN -> LDIF -> Maybe LDIFRecord -> LDIF
applyChange2Record (ChangeAdd vals)   dn ld Nothing  = LDIFContent (lcVersion ld) ((lcEntries ld)++[ContentRecord dn vals]) 
applyChange2Record (ChangeAdd _)      dn _  (Just _) = error ("ADD: Already exists: "++(show dn))
applyChange2Record ChangeDelete       dn ld (Just _) = let rn = filter (\x -> dn /= (reDN x)) (lcEntries ld)
                                                       in LDIFContent (lcVersion ld) rn
applyChange2Record ChangeDelete       dn _  Nothing  = error ("DELETE: Entry not found: "++(show dn))
applyChange2Record (ChangeModify ops) dn ld (Just r) = let pre  = takeWhile (\x -> dn /= (reDN x)) (lcEntries ld)
                                                           post = filter (\x -> dn /= (reDN x)) $ dropWhile (\x -> dn /= (reDN x)) (lcEntries ld)
                                                           rn   = foldr applyMod2Record r ops
                                                       in LDIFContent (lcVersion ld) (pre++[rn]++post)
applyChange2Record ChangeModDN      _ _ _  = error "Operation ModDN is not supported"
applyChange2Record _ _ _ _ = error $ "Unexpected LDIF Content"

-- | Apply Attribute Modification (Add/Del/Replace) to ContentRecord and produce changed ContentRecord
applyMod2Record :: Modify -> LDIFRecord -> LDIFRecord
applyMod2Record (ModAdd      name vals) (ContentRecord dn av) = let nav = map (\v -> (name,v)) vals  -- new attr/values
                                                                    mav = av ++ nav                  -- merged attr/values
                                                                    verified = if (length $ nub mav) == (length mav) then mav
                                                                               else error ("ModAdd: Values already exists: "++(show av)++" vs "++(show vals)++" DN:"++(show dn))
                                                                    in ContentRecord dn verified
applyMod2Record (ModDelete   name [])   (ContentRecord dn av) = let nav = filter (\(n,_) -> n /= name) av -- new attr/values
                                                                    verified = if (length $ nub nav) /= (length av) then nav
                                                                               else error ("ModDel: Attribute not found: "++(show name)++" DN:"++(show dn))
                                                                in ContentRecord dn verified
applyMod2Record (ModDelete   name vals) (ContentRecord dn av) = let mav = filter (\(n,v) -> n /= name || v `notElem` vals) av
                                                                    verified = if (length av) - (length mav) == (length vals) then mav
                                                                               else error ("ModDel: Attribute/Value not found: "++(show name)++"vals"++(show vals)++" DN:"++(show dn))
                                                                in ContentRecord dn verified
applyMod2Record (ModReplace  name vals) (ContentRecord dn av) = let nav = map (\v -> (name,v)) vals -- new attr/values
                                                                    mav = (filter (\(n,_) -> n /= name) av) ++ nav -- merged
                                                                in ContentRecord dn mav
applyMod2Record _ x = error $ "Unexpected LDIF Record:" ++ (show x)
