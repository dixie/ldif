{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | LDIF representation in Data.Tree structure
module Text.LDIF.Tree
       where
import Prelude
import Text.LDIF.Types
import Text.LDIF.Utils
import Data.Tree
import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Tree.Zipper as Z

-- | Convert Tree of Records to LDIF
fromTree :: Tree LDIFRecord -> LDIF
fromTree !xs = ys `seq` LDIF Nothing ys
  where
    ys = (filter (not . isFakeEntry) $ flatten xs)
      where
        isFakeEntry (ContentRecord _ []) = True
        isFakeEntry _ = False

-- | Convert LDIF to Tree using DNs. It can construct missing parents as an dummy records.
toTree :: LDIF -> Bool -> Tree LDIFRecord
toTree (LDIF _ entrs) fp = fromRecords entries'
  where
    entries' = if fp then addFakeParents entrs else entrs
      where
        addFakeParents entries = (fakeEntries ++ entries)
          where 
            fakeEntries = sortBy compareByDNLen $ map fakeEntry missingDNs
              where
                fakeEntry dn = ContentRecord dn []
                compareByDNLen a b = (lengthOfDN $ reDN a) `compare` (lengthOfDN $ reDN b)
                missingDNs = filter ((flip S.notMember) allDNs) $ S.toList parentDNs
                  where
                    allDNs = S.fromList $ map reDN entries
                    parentDNs =  S.fromList $ map DN $ filter (not . null) $ concatMap (tails . dnAttrVals) $ S.toList allDNs
    fromRecords !xs = Z.toTree $ foldl' (\tree entry -> addEntry tree entry) rootEntry xs
      where
        rootEntry = Z.fromTree $ Node (ContentRecord (DN []) []) []
        addEntry !tree !entry = Z.root $ Z.insert (Node entry []) (findParent tree)
          where
            findParent !z | not $ Z.hasChildren z = Z.children z -- No children; put it here
                          | isNothing child       = Z.children z -- No matching child; put it here
                          | otherwise             = findParent (fromJust child) -- found matching child, continue
              where
                child = findChild (Z.firstChild z) -- Traverse all childs
                  where
                    findChild !Nothing  = Nothing -- Nothing found
                    findChild !(Just c)  | (Z.label c) `isParentRecordOf` entry = Just c  -- Found
                                         | otherwise                            = findChild (Z.next c) -- Continue
