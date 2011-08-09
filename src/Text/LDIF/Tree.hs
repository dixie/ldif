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
            allDNs = S.fromList $ map reDN entries
            parentDNs =  S.fromList $ map DN $ filter (not . null) $ concatMap (tails . dnAttrVals) $ S.toList allDNs
            missingDNs = filter ((flip S.notMember) allDNs) $ S.toList parentDNs
            fakeEntries = sortBy compareByDNLen $ map fakeEntry missingDNs
              where
                fakeEntry dn = ContentRecord dn []
                compareByDNLen a b = (lengthOfDN $ reDN a) `compare` (lengthOfDN $ reDN b)
    fromRecords !xs = Z.toTree $ foldl (\t n -> addNode t n) dummyRoot xs
      where
        dummyRoot = Z.fromTree $ Node (ContentRecord dummyRootDN []) []
          where
            dummyRootDN = DN []
        addNode !t !n = Z.root $ Z.insert (Node n []) (findParent t)
          where
            findParent !z = if not $ Z.hasChildren z then Z.children z 
                             else if isNothing z' then Z.children z else findParent (fromJust z')
              where
                z' = findChild (Z.firstChild z)
                  where
                    findChild !Nothing  = Nothing
                    findChild !(Just c) = if (Z.label c) `isParentRecordOf` n then Just c else findChild (Z.next c)
