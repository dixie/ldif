-- | LDIF related operations
module Text.LDIF.Proc (
        findChangesByDN,
        findContentsByDN,
	findContentByDN,
	diffLDIF,
        diffRecord 
)
where
import Text.LDIF.Types

-- | Find all Changes with given DN
findChangesByDN :: LDIF -> DN -> [ChangeRecord]
findChangesByDN _ _ = error "not implemented"

-- | Find all Contents with given DN
findContentsByDN :: LDIF -> DN -> [ContentRecord]
findContentsByDN (LDIFContent _ entries) dn = filter (\x -> (coDN x) == dn) entries
findContentsByDN _ _ = []

-- | Find first Content with given DN
findContentByDN :: LDIF -> DN -> Maybe ContentRecord
findContentByDN ldif dn = case findContentsByDN ldif dn of
                                 []   -> Nothing
                                 xs   -> Just (head xs)

-- | Create Change LDIF between to LDIF contents 
diffLDIF :: LDIF -> LDIF -> Maybe LDIF
diffLDIF l1@(LDIFContent v1 c1) l2@(LDIFContent v2 c2) = Just (LDIFChanges v2 changes) 
   where 
      changes = [] -- foldl () [] 
diffLDIF _ _ = Nothing

-- | Diff two AttrVal Records if any of provided 
diffRecord :: ContentRecord -> ContentRecord -> Maybe ChangeRecord
diffRecord r1 r2 | (coDN r1) /= (coDN r2) = Nothing
                 | otherwise = Just (ChangeRecord (coDN r1) (ChangeModify mods))
   where
      mods = delMods ++ addMods
      addMods = map (\x -> ModAdd (fst x) [(snd x)]) addVals
      delMods = map (\x -> ModDelete (fst x) [(snd x)]) delVals
      addVals = filter (\x -> not $ elem x (coAttrVals r1)) (coAttrVals r2) :: [AttrValue]
      delVals = filter (\x -> not $ elem x (coAttrVals r2)) (coAttrVals r1) :: [AttrValue]
