{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Text.LDIF.Preproc ( preproc 
                         , transposePos 
                         , PosTable )
where
import Text.Parsec
import Text.Parsec.Error (setErrorPos)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

-- | Contains Mapping between Position in preprocessed text to original text Position
type Position = (Line,Column) 
type PosDeltaLine = Int
type PosDeltaCol  = Int

-- | Opaque data necessary for relation between text after preprocessing and original
data PosTable = PosTable (M.Map Position PosDeltaLine) (M.Map Position PosDeltaCol)

findDeltaLine :: PosTable -> Position -> PosDeltaLine
findDeltaLine (PosTable ptabLine _) pos = M.findWithDefault 0 pos ptabLine

findDeltaCol :: PosTable -> Position -> PosDeltaCol
findDeltaCol (PosTable _ ptabCol) pos = M.findWithDefault 0 pos ptabCol

-- | Convert error position to original text before preprocessing
transposePos :: PosTable -> ParseError -> ParseError
transposePos ptab oe = setErrorPos npos oe
  where
    npos = setSourceColumn (setSourceLine opos nlin) ncol
      where
        opos = errorPos oe        
        olin = sourceLine opos
        ocol = sourceColumn opos
        ncol = ocol + dcol
          where
            dcol = findDeltaCol ptab (olin,ocol) 
        nlin = olin + dlin
          where
            dlin = findDeltaLine ptab (olin,ocol)
            
-- | Preprocessing for concat wrapped lines and remove comment lines
preproc :: BC.ByteString -> (BC.ByteString, PosTable)
preproc xs = (BC.unlines ys, ptab)
  where 
    (ys, ptab) = stripComments $ unwrap $ (specLines xs, PosTable M.empty M.empty)

specLines :: BC.ByteString -> [BC.ByteString]
specLines xs = map cleanLine $ BC.lines xs
  where
    cleanLine l = BC.reverse $ BC.dropWhile (isCR) (BC.reverse l)
      where
        isCR c = c == '\r'

-- | Remove Comment Lines
stripComments :: ([BC.ByteString],PosTable) -> ([BC.ByteString], PosTable)
stripComments (input, ptab) = (filter (not . BC.isPrefixOf "#") input, ptab)

-- | Unwrap lines, lines with space at begin is continue of previous line 
unwrap :: ([BC.ByteString],PosTable) -> ([BC.ByteString],PosTable)
unwrap (xs, ptab) = (takeLines xs, ptab)

takeLines :: [BC.ByteString] -> [BC.ByteString]
takeLines [] = []
takeLines xs = let (ln,ys) = takeLine xs
               in ln:takeLines ys

takeLine :: [BC.ByteString] -> (BC.ByteString, [BC.ByteString])
takeLine []  = (BC.empty,[])
takeLine (x:[]) = (x,[])
takeLine (x:xs) = let isCont z = " " `BC.isPrefixOf` z
                  in (x `BC.append` (BC.concat $ map (BC.tail) $ takeWhile (isCont) xs), dropWhile (isCont) xs) 

