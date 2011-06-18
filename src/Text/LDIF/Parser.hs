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
import Text.Parsec.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BC
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
    pSEPs
    recs <- sepEndBy pChangeRec pSEPs
    eof
    return $ LDIF ver recs

pLdifMixed:: Parser LDIF
pLdifMixed = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    pSEPs
    recs <- sepEndBy pRec pSEPs
    eof
    return $ LDIF ver recs

pLdifContent :: Parser LDIF
pLdifContent = do
    pSEPs
    ver <- optionMaybe pVersionSpec
    pSEPs
    recs <- sepEndBy pAttrValRec pSEPs
    eof
    return $ LDIF ver recs

pAttrValRec ::  Parser LDIFRecord
pAttrValRec = do
    dn <- pDNSpec
    pSEP
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ ContentRecord dn attrVals

pRec :: Parser LDIFRecord
pRec = try pChangeRec <|> pAttrValRec

pChangeRec :: Parser LDIFRecord
pChangeRec = try pChangeAdd
         <|> try pChangeDel
         <|> try pChangeMod
         <|> pChangeModDN

pChangeAdd :: Parser LDIFRecord
pChangeAdd = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
    _ <- string "add"
    pSEP
    vals <- sepEndBy1 pAttrValSpec pSEP
    return $ ChangeRecord dn (ChangeAdd vals)

pChangeDel :: Parser LDIFRecord
pChangeDel = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
    _ <- string "delete"
    pSEP
    return $ ChangeRecord dn ChangeDelete

pChangeMod :: Parser LDIFRecord
pChangeMod = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
    _ <- string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ ChangeRecord dn (ChangeModify mods)

pChangeModDN :: Parser LDIFRecord
pChangeModDN = do
    dn <- pDNSpec
    pSEP
    _ <- string "changetype:"
    pFILL
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
   return $ DN avals

pAttrEqValue :: Parser AttrValue
pAttrEqValue = do
   pFILL
   att <- pAttributeType
   _ <- char '='
   val <- pAttrValueDN
   return (att,val)

pAttrValueDN :: Parser Value
pAttrValueDN = do
   xs <- many allChar
   let ys = BC.pack xs
   ys `seq` return $ ys
   where 
     allChar = try (escChar) 
               <|> try (hexChar) 
               <|> (noneOf (escapedDNChars ++ "\n\r"))
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
   let ys = BC.pack xs
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
             <|> (do { l <- letter; o <- pAttrTypeChars; return (Attribute $ l `BC.cons` o) } )

pAttrValSpec :: Parser AttrValue
pAttrValSpec = do
   name <- pAttributeDescription
   val  <- pValueSpec
   return (name, val)

pValueSpec :: Parser Value
pValueSpec = try (char ':' >> char ':' >> pFILL >> pBase64String)
         <|> try (char ':' >> pFILL >> pSafeString') 
         <|> (char ':' >> char '<' >> pFILL >> pURL)

pURL :: Parser BC.ByteString
pURL = pSafeString

pSafeString :: Parser BC.ByteString
pSafeString = do
   c <- noneOf "\n\r :<"
   r <- many (noneOf "\n\r")   
   let ys = BC.pack $ c:r
   ys `seq` return ys

pSafeString' :: Parser BC.ByteString
pSafeString' = do
   r <- many (noneOf "\n\r")
   let ys = BC.pack r
   ys `seq` return ys
 
pBase64String :: Parser BC.ByteString
pBase64String = pSafeString

pAttrTypeChars :: Parser BC.ByteString
pAttrTypeChars = do 
  xs <- many (satisfy (\x -> isAlphaNum x || x == '-'))
  let ys = BC.pack xs
  ys `seq` return ys


pLdapOid :: Parser Attribute
pLdapOid = do
   num <- many1 digit
   rest <- many (do { _ <- string "."; n <- many1 digit; return $ '.':n})
   return (Attribute $ BC.pack $ num ++ concat rest)

pFILL :: Parser ()
pFILL = skipMany (oneOf [' ', '\t'])

pSEP :: Parser ()
pSEP = try (char '\r' >> char '\n' >> return () )
   <|> (char '\n' >> return () )

pSEPs :: Parser ()
pSEPs = many pSEP >> return ()

