module Text.LDIF.Proc (
	diffLDIF,
        diffRecord 
)
where
import Text.LDIF.Types

-- | Create Change LDIF between to LDIF contents 
diffLDIF :: LDIF -> LDIF -> Maybe LDIF
diffLDIF l1 l2 = error "not implemeneted"


-- | Diff two AttrVal Records if any of provided 
diffRecord :: ContentRecord -> ContentRecord -> Maybe ChangeRecord
diffRecord r1 r2 | (coDN r1) /= (coDN r2) = Nothing
                 | otherwise = Just (ChangeRecord (coDN r1) (ChangeModify mods))
   where
      mods = delMods ++ addMods
      addMods = map (\x -> ModAdd (fst x) [(snd x)]) addVals
      delMods = map (\x -> ModDelete (fst x) [(snd x)]) delVals
      addVals = filter (\x -> elem x (coAttrVals r1)) (coAttrVals r2) :: [AttrValue]
      delVals = filter (\x -> elem x (coAttrVals r2)) (coAttrVals r1) :: [AttrValue]
