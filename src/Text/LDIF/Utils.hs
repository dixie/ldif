-- | LDIF related operations
module Text.LDIF.Utils (
        findRecordsByDN,
	findRecordByDN,
        isDNPrefixOf,
        sizeOfDN,
        takeDNPrefix,
        leafOfDN,
        rootOfDN,
        lookupAttr,
        filterAttr,
        isDummyRecord,
        ldif2tree
)
where
import Text.LDIF.Types
import Text.LDIF.Printer
import Data.Maybe
import Data.Either
import Data.Tree
import Data.List (nub)

-- | Find all Contents with given DN
findRecordsByDN :: LDIF -> DN -> [LDIFRecord]
findRecordsByDN (LDIFContent _ entries) dn = filter (\x -> (reDN x) == dn) entries
findRecordsByDN (LDIFChanges _ entries) dn = filter (\x -> (reDN x) == dn) entries

-- | Find first Content with given DN
findRecordByDN :: LDIF -> DN -> Maybe LDIFRecord
findRecordByDN ldif dn = case findRecordsByDN ldif dn of
                                 []   -> Nothing
                                 xs   -> Just (head xs)


-- | Find fist Attribute within attributes pairs list
lookupAttr :: String -> [AttrValue] -> Maybe Value
lookupAttr attr xs = lookup (Attribute attr) xs

-- | Filter Attribute Value list according Attribute name
filterAttr :: String -> [AttrValue] -> [AttrValue]
filterAttr attr xs  = filter (\x -> (Attribute attr) == fst x) xs

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

dummyRootDN = DN [(Attribute "dc", "root")]

ldif2tree :: LDIF -> Tree LDIFRecord
ldif2tree (LDIFContent _ entries) = Node (ContentRecord dummyRootDN []) (ldifRecs2tree entries)
ldif2tree (LDIFChanges _ entries) = Node (ChangeRecord dummyRootDN (ChangeAdd [])) (ldifRecs2tree entries)

isParentRecordOf :: LDIFRecord -> LDIFRecord -> Bool
isParentRecordOf a b = isDNPrefixOf (reDN a) (reDN b)

ldifRoots xs = let isRoot x = all (\y -> not $ isParentRecordOf y x) xs
               in filter (isRoot) xs

ldifRecs2tree :: [LDIFRecord] -> [Tree LDIFRecord]
ldifRecs2tree xs = let roots = (ldifRoots xs)
                       subtr x = ldifRecs2tree $ filter (isParentRecordOf x) xs
                   in map (\x -> Node x (subtr x)) roots
