{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Text.LDIF.Parser (
	parseLDIFStr,
        parseLDIFStrAs,
	parseLDIFFile,
        parseDNStr
)
where
import Prelude
import Text.LDIF.Types
import Text.LDIF.Consts
import Text.Parsec as PR
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Numeric (readHex)

-- | Parse string as LDIF content and return LDIF or ParseError
parseLDIFStr :: BC.ByteString -> Either ParseError LDIF
parseLDIFStr = parseLDIFStrAs Nothing 

-- | Read and parse provided file and return LDIF or ParseError
parseLDIFFile :: String -> IO (Either ParseError LDIF)
parseLDIFFile name = do
	input <- BC.readFile name
        return $ parseLDIFStrAs' name Nothing input

-- | Read and parse provided string and return LDIF or ParserError
-- | If LDIF type is specified than given type is expected for parsing 
-- | and mismatch generates ParseError
parseLDIFStrAs' :: String -> Maybe LDIFType -> BC.ByteString -> Either ParseError LDIF
parseLDIFStrAs' nm Nothing                xs = parse pLdif        nm $ preproc xs
parseLDIFStrAs' nm (Just LDIFMixedType)   xs = parse pLdif        nm $ preproc xs
parseLDIFStrAs' nm (Just LDIFContentType) xs = parse pLdifContent nm $ preproc xs
parseLDIFStrAs' nm (Just LDIFChangesType) xs = parse pLdifChanges nm $ preproc xs

parseLDIFStrAs :: Maybe LDIFType -> BC.ByteString -> Either ParseError LDIF
parseLDIFStrAs = parseLDIFStrAs' "(param)"

-- | Parse string as DN and return DN type or ParseError
parseDNStr :: BC.ByteString -> Either ParseError DN
parseDNStr = parse pDN "(param)" 

-- | Preprocessing for concat wrapped lines and remove comment lines
preproc :: BC.ByteString -> BC.ByteString
preproc xs = BC.unlines $ stripComments $ unwrap $ BC.lines xs

-- | Remove Comment Lines
stripComments :: [BC.ByteString] -> [BC.ByteString]
stripComments input = filter (not . BC.isPrefixOf "#") input

-- | Unwrap lines, lines with space at begin is continue of previous line 
unwrap :: [BC.ByteString] -> [BC.ByteString]
unwrap xs = takeLines xs

takeLines :: [BC.ByteString] -> [BC.ByteString]
takeLines [] = []
takeLines xs = let (ln,ys) = takeLine xs
               in ln:takeLines ys

takeLine :: [BC.ByteString] -> (BC.ByteString, [BC.ByteString])
takeLine []  = (BC.empty,[])
takeLine (x:[]) = (x,[])
takeLine (x:xs) = let isCont z = " " `BC.isPrefixOf` z
                  in (x `BC.append` (BC.concat $ map (BC.tail) $ takeWhile (isCont) xs), dropWhile (isCont) xs) 

-- | Parsec ldif parser
pLdif :: Parser LDIF
pLdif = try pLdifChanges <|> pLdifMixed

pLdifChanges :: Parser LDIF
pLdifChanges = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy pChangeRec pSEPs1
    _ <- optionMaybe pSearchResult
    eof
    return $ LDIF ver recs

pLdifMixed:: Parser LDIF
pLdifMixed = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy pRec pSEPs1
    _ <- optionMaybe pSearchResult
    eof
    recs `seq` return $ LDIF ver recs

pLdifContent :: Parser LDIF
pLdifContent = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy pAttrValRec pSEPs1
    _ <- optionMaybe pSearchResult
    eof
    return $ LDIF ver recs

pAttrValRec ::  Parser LDIFRecord
pAttrValRec = do
    dn <- pDNSpec
    pSEP
    pAttrValRec' dn
    
pAttrValRec' :: DN -> Parser LDIFRecord
pAttrValRec' dn = do
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    attrVals `seq` return $ ContentRecord dn attrVals

pRec :: Parser LDIFRecord
pRec = do 
  dn <- pDNSpec
  pSEP
  try (pChangeRec'' dn) <|> (pAttrValRec' dn)

pChangeRec :: Parser LDIFRecord
pChangeRec = do
  dn <- pDNSpec
  pSEP
  pChangeRec'' dn

pChangeRec'' :: DN -> Parser LDIFRecord
pChangeRec'' dn = do
  _ <- string "changetype:"
  pFILL
  pChangeRec' dn

pChangeRec' :: DN -> Parser LDIFRecord
pChangeRec' dn = try (pChangeAdd dn)
                 <|> try (pChangeDel dn)
                 <|> try (pChangeMod dn)
                 <|> (pChangeModDN dn)

pChangeAdd :: DN -> Parser LDIFRecord
pChangeAdd dn  = do
    _ <- string "add"
    pSEP
    vals <- sepEndBy1 pAttrValSpec pSEP
    return $ ChangeRecord dn (ChangeAdd vals)

pChangeDel :: DN -> Parser LDIFRecord
pChangeDel dn = do
    _ <- string "delete"
    pSEP
    return $ ChangeRecord dn ChangeDelete

pChangeMod :: DN -> Parser LDIFRecord
pChangeMod dn = do
    _ <- string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ ChangeRecord dn (ChangeModify mods)

pChangeModDN :: DN -> Parser LDIFRecord
pChangeModDN dn = do
    _ <- string "modrdn" 
    pSEP
    _ <- string "newrdn:"
    pFILL 
    _ <- pRDN
    pSEP
    _ <- string "deleteoldrdn:"
    pFILL
    _ <- oneOf "01"
    pSEP
    return $ ChangeRecord dn ChangeModDN

pRDN :: Parser BC.ByteString
pRDN = pSafeString

pDNSpec :: Parser DN
pDNSpec = do
    _ <- string "dn:"
    pDN

pDN :: Parser DN
pDN = do
   pFILL
   avals <- sepEndBy pAttrEqValue (char ',')
   avals `seq` return $ DN avals

pAttrEqValue :: Parser AttrValue
pAttrEqValue = do
   pFILL
   att <- pAttributeType
   _ <- char '='
   val <- pAttrValueDN
   att `seq` val `seq` return (att,val)

pAttrValueDN :: Parser Value
pAttrValueDN = do
   xs <- many1 allChar
   let ys = xs `seq` (Value $ BC.pack xs)
   ys `seq` return $ ys
   where 
     allChar = noneOf (escapedDNChars ++ "\n\r")
               <|> try (hexChar)
               <|> (escChar)
     escChar = do
       _ <- char '\\'
       oneOf escapedDNChars
     hexChar = do
       _ <- char '\\'
       hval <- PR.count 2 hexDigit
       case readHex hval of
         [(val,[])] -> return $ chr val
         _          -> fail $ "invalid hex value: " ++ hval

pVersionSpec :: Parser BC.ByteString
pVersionSpec = do
   _ <- string "version:"
   pFILL
   xs <- many1 digit
   pSEPs1
   let ys = xs `seq` BC.pack xs
   ys `seq` return $ ys

pModSpec :: Parser Modify
pModSpec = do
   modType <- pModType
   pFILL
   att <- pAttributeDescription 
   pSEP 
   vals <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modType att vals

mkMod :: String -> Attribute -> [AttrValue] -> Modify
mkMod modType att vals | modType == "add:" = ModAdd att (map (snd) vals)
                       | modType == "delete:" = ModDelete att (map (snd) vals)
                       | modType == "replace:" = ModReplace att (map (snd) vals)
                       | otherwise = error $ "unexpected mod:" ++ modType
                         -- error can not be reached because pModType

pModType :: Parser String
pModType = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeDescription :: Parser Attribute
pAttributeDescription = pAttributeType

pAttributeType :: Parser Attribute
pAttributeType = try pLdapOid
             <|> pCharType
   where
      pDotOid = do
         _ <- string "." 
         n <- many1 digit
         let xs = n `seq` '.':n
         xs `seq` return xs
      pLdapOid = do
        num <- many1 digit
        rest <- many1 pDotOid
        let xs = num `seq` rest `seq` num ++ concat rest
        xs `seq` return (Attribute $ BC.pack xs)
      pCharType = do
         l <- letter 
         o <- pAttrTypeChars
         let xs = l `seq` o `seq` l `BC.cons` o
         xs `seq` return $ Attribute xs
           where
             pAttrTypeChars :: Parser BC.ByteString
             pAttrTypeChars = do 
               xs <- many (satisfy (\x -> isAlphaNum x || x == '-'))
               let ys = xs `seq` BC.pack xs
               ys `seq` return ys


pAttrValSpec :: Parser AttrValue
pAttrValSpec = do
   name <- pAttributeDescription
   val  <- pValueSpec
   name `seq` val `seq` return (name, val)
     where
       pValueSpec :: Parser Value
       pValueSpec = try (char ':' >> pFILL >> pSafeString' >>= (\x -> return $ Value x))
                    <|> try (char ':' >> char ':' >> pFILL >> pBase64String >>= (\x -> return $ Value x))
                    <|> (char ':' >> char '<' >> pFILL >> pURL >>= (\x -> return $ Value x))

pURL :: Parser BC.ByteString
pURL = pSafeString

pSafeString :: Parser BC.ByteString
pSafeString = do
   c <- noneOf "\n\r :<"
   r <- many (noneOf "\n\r")   
   let xs = r `seq` c:r
   let ys = xs `seq` BC.pack xs
   ys `seq` return ys

pSafeString' :: Parser BC.ByteString
pSafeString' = do
   r <- many (noneOf "\n\r")
   let ys = r `seq` BC.pack r
   ys `seq` return ys
 
pBase64String :: Parser BC.ByteString
pBase64String = pSafeString

pFILL :: Parser ()
pFILL = skipMany (oneOf [' ', '\t'])

pSEP :: Parser ()
pSEP = try (char '\r' >> char '\n' >> return () )
   <|> (char '\n' >> return () )

pSEPs :: Parser ()
pSEPs = many pSEP >> return ()

pSEPs1 :: Parser ()
pSEPs1 = many1 pSEP >> return ()

pSearchResult :: Parser ()
pSearchResult = do
   _ <- string "search:"
   pFILL
   _ <- many1 digit
   pSEP
   _ <- string "result:"
   pFILL
   _ <- pSafeString
   pSEPs
   return ()
