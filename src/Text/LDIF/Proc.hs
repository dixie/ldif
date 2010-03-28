-- | LDIF related operations
module Text.LDIF.Proc (
        findRecordsByDN,
	findRecordByDN,
	diffLDIF,
        diffRecord,
        applyLDIF,
        isDNPrefixOf,
        sizeOfDN,
        takeDNPrefix,
        leafOfDN,
        rootOfDN
)
where
import Text.LDIF.Types
import Text.LDIF.Printer
import Data.Maybe
import Data.Either

-- | Find all Contents with given DN
findRecordsByDN :: LDIF -> DN -> [LDIFRecord]
findRecordsByDN (LDIFContent _ entries) dn = filter (\x -> (coDN x) == dn) entries
findRecordsByDN (LDIFChanges _ entries) dn = filter (\x -> (chDN x) == dn) entries

-- | Find first Content with given DN
findRecordByDN :: LDIF -> DN -> Maybe LDIFRecord
findRecordByDN ldif dn = case findRecordsByDN ldif dn of
                                 []   -> Nothing
                                 xs   -> Just (head xs)

-- | Apply one LDIF to another LDIF. The destination LDIF has
-- | to be Content LDIF
applyLDIF :: LDIF -> LDIF -> LDIF
applyLDIF dst@(LDIFContent _ _) (LDIFChanges _ xs) = foldr (applyRecord2LDIF) dst xs
applyLDIF dst@(LDIFContent _ _) (LDIFContent _ xs) = foldr (applyRecord2LDIF) dst xs
applyLDIF _ _ = error "Destination LDIF has to be Content LDIF and not Change LDIF"

applyRecord2LDIF :: LDIFRecord -> LDIF -> LDIF
applyRecord2LDIF rec@(ContentRecord dn vals) dst = applyRecord2LDIF (ChangeRecord dn (ChangeAdd vals)) dst
applyRecord2LDIF rec@(ChangeRecord  dn op)   dst = applyChange2Record op dn dst (findRecordByDN dst dn)

applyChange2Record :: Change -> DN -> LDIF -> Maybe LDIFRecord -> LDIF
-- applyChange2Record op dn dst dstRec = undefined
applyChange2Record (ChangeAdd vals) dn (LDIFContent v xs) Nothing  = LDIFContent v (xs++[ContentRecord dn vals]) 
applyChange2Record (ChangeAdd vals) dn (LDIFContent v xs) (Just _) = error "already exists"
applyChange2Record ChangeDelete     dn (LDIFContent v xs) (Just _) = LDIFContent v (filter (\x -> dn /= (coDN x)) xs)
applyChange2Record ChangeDelete     dn (LDIFContent v xs) Nothing  = error "not found"
applyChange2Record ChangeModDN      _ _ _  = error "Operation ModDN is not supported"


-- | Create Change LDIF between to LDIF contents. If any
-- | of input argument is not LDIFContent it returns Nothing. 
-- | If there is not difference the Change LDIF with empty
-- | change list is returned.
-- |
-- | Unsing following strategy: 
-- | 1. Iterate over L1 DN's and Modify / Remove Content 
-- | 2. Iterate over L2 and Add Content not in L1
diffLDIF :: LDIF -> LDIF -> Maybe LDIF
diffLDIF l1@(LDIFContent _ c1) l2@(LDIFContent v2 c2) = Just (LDIFChanges v2 (changes ++ adds))
   where 
      adds = map (content2add) $ filter (not . isEntryIn l1) c2
      changes = filter (not . isDummyRecord) $ foldl (processEntry) [] c1
      processEntry xs e1 = let me2 = findRecordByDN l2 (coDN e1) 
                               change = case me2 of
					   Nothing -> ChangeRecord (coDN e1) ChangeDelete
                                           Just e2 -> fromJust $ diffRecord e1 e2
                           in xs ++ [change]
      isEntryIn ll ex = let mex = findRecordByDN ll (coDN ex)
                        in case mex of
                          Nothing -> False
                          Just _  -> True
      content2add (ContentRecord dn vals) = ChangeRecord dn (ChangeAdd vals)
diffLDIF _ _ = Nothing

-- | Diff two AttrVal Records if any of provided. 
-- | Implementation uses inefficient algorithm for large count of attributes within ContentRecord.
diffRecord :: LDIFRecord -> LDIFRecord -> Maybe LDIFRecord
diffRecord r1 r2 | (coDN r1) /= (coDN r2) = Nothing
                 | otherwise = Just (ChangeRecord (coDN r1) (ChangeModify mods))
   where
      mods = delMods ++ addMods
      addMods = map (\x -> ModAdd (fst x) [(snd x)]) addVals
      delMods = map (\x -> ModDelete (fst x) [(snd x)]) delVals
      addVals = filter (\x -> not $ elem x (coAttrVals r1)) (coAttrVals r2) :: [AttrValue]
      delVals = filter (\x -> not $ elem x (coAttrVals r2)) (coAttrVals r1) :: [AttrValue]

appendLDIF :: LDIF -> LDIFRecord -> LDIF
appendLDIF (LDIFContent v ys) x@(ContentRecord _ _) = LDIFContent v (ys++[x])
appendLDIF (LDIFChanges v ys) x@(ChangeRecord  _ _) = LDIFChanges v (ys++[x])

-- | Change record without any impact
isDummyRecord :: LDIFRecord -> Bool
isDummyRecord (ChangeRecord _ (ChangeModify [])) = True 
isDummyRecord _ = False

leafOfDN :: DN -> AttrValue
leafOfDN xs = getDNValue xs 0

rootOfDN :: DN -> AttrValue
rootOfDN xs = getDNValue xs ((sizeOfDN xs)-1)

sizeOfDN :: DN -> Int
sizeOfDN (DN vals) = length vals

getDNValue :: DN -> Int -> AttrValue
getDNValue (DN vals) idx = vals !! idx

takeDNPrefix :: DN -> Int -> DN
takeDNPrefix (DN vals) n = (DN (take n vals))

-- | Check if the dn1 is prefix of dn2
isDNPrefixOf :: DN -> DN -> Bool
isDNPrefixOf dn1 dn2 | (sizeOfDN dn1) >= (sizeOfDN dn2) = False
                     | otherwise = let n = (sizeOfDN dn2)
                                   in (takeDNPrefix dn1 n) == dn2

