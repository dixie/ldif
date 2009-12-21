module Text.LDIF (
	parseLDIF,
 	LDIF(..),   
        Record(..),
        Change(..),
        Modify(..), 
        DN, Attribute, Value, AttrValue
)
where
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Char

type Attribute = String
type Value = String
type AttrValue = (Attribute, Value)
type DN = String

data LDIF = LDIFContent { lcVersion :: Maybe String, lcEntries :: [Record] }
          | LDIFChanges { lcVersion :: Maybe String, lcEntries :: [Record] } 

data Record = AttrValRecord { recDN :: DN, recAttrVals :: [AttrValue] }  
	    | ChangeRecord  { recDN :: DN, recOp :: Change }

data Change = ChangeAdd     { chAttrVals :: [AttrValue] }
            | ChangeDelete 
            | ChangeModify  { chMods :: [Modify] }
            | ChangeModDN 

data Modify = ModAdd     { modAttr :: Attribute, modAttrVals :: [AttrValue] }
            | ModDelete  { modAttr :: Attribute, modAttrVals :: [AttrValue] }
            | ModReplace { modAttr :: Attribute, modAttrVals :: [AttrValue] }

parseLDIF :: String -> Either ParseError LDIF
parseLDIF input = parse pLdif "(param)" input

-- | Parsec ldif parser
pLdif :: CharParser st LDIF
pLdif = do
    try (pLdifContent) 
    <|> pLdifChanges

pLdifChanges :: CharParser st LDIF
pLdifChanges = do
    ver <- optionMaybe (pVersionSpec)
    recs <- many1 (pChangeRec)
    return $ LDIFChanges ver recs

pLdifContent :: CharParser st LDIF
pLdifContent = do
    ver <- optionMaybe (pVersionSpec)
    recs <- many1 (pAttrValRec)
    return $ LDIFContent ver recs

pAttrValRec ::  CharParser st Record
pAttrValRec = do
    dn <- pDNSpec
    attrVals <- many1 (pAttrValSpec)
    return $ AttrValRecord dn attrVals

pChangeRec :: CharParser st Record
pChangeRec = do
    string "changetype:"
    pFILL
    try (pChangeAdd) 
    <|> try (pChangeDel)
    <|> try (pChangeMod)
    <|> pChangeModDN

pChangeAdd :: CharParser st Change
pChangeAdd = do
     string "add"
     pSEP
     vals <- many1 (pAttrValSpec)
     return $ ChangeAdd vals

pChangeDel :: CharParser st Change
pChangeDel = do
     string "delete"
     pSEP
     return $ ChangeDelete

pChangeMod :: CharParser st Change
pChangeMod = do
     string "modify"
     pSEP
     mods <- many (pModSpec)
     return $ ChangeModify mods

pChangeModDN :: CharParser st Change
pChangeModDN = do
     string "modrdn" 
     pSEP
     string "newrdn:"
     pFILL 
     pRDN
     pSEP
     string "deleteoldrdn:"
     pFILL
     oneOf "01"
     pSEP
     return $ ChangeModDN

pRDN = pSafeString

pDNSpec :: CharParser st DN
pDNSpec = do
    string "dn:"
    pFILL
    pSafeString

pVersionSpec :: CharParser st String
pVersionSpec = do
   string "version:"
   pFILL
   many1 (digit)

pModSpec :: CharParser st (String,String)
pModSpec = do
   mod <- pModType
   pFILL
   att <- pAttributeDescription 
   pSEP 
   vals <- pAttrValSpec
   return $ mkMod mod att vals

mkMod mod att vals | mod == "add:" = ModAdd att vals
                   | mod == "delete:" = ModDelete att vals
                   | mod == "replace:" = ModReplace att vals
                   | otherwise = error $ "unexpected mod:" ++ mod

pModType :: CharParser st String
pModType = do
       try (string "add:")
   <|> try (string "delete:")
   <|> string "replace:"

pAttributeDescription :: CharParser st String
pAttributeDescription = do
   pAttributeType

pAttributeType :: CharParser st String
pAttributeType = do
   try (pLdapOid)
   <|> (do { l <- letter; o <- pAttrTypeChars; return $ l:o } )

pAttrValSpec :: CharParser st AttrValue
pAttrValSpec = do
   name <- pAttributeDescription
   val  <- pValueSpec
   return $ (name, val)

pValueSpec :: CharParser st Value
pValueSpec = do
   try (char ':' >> char ':' >> pFILL >> pBase64String)
   <|> try (char ':' >> pFILL >> pSafeString) 
   <|> (char ':' >> char '<' >> pFILL >> pURL)

pURL = pSafeString

pSafeString :: CharParser st String
pSafeString = do
   c <- noneOf "\n\r :<"
   r <- many (noneOf ("\n\r"))
   return $ c:r
 
pBase64String = pSafeString

pAttrTypeChars :: CharParser st String
pAttrTypeChars = do 
   many (satisfy (\x -> isAlphaNum x || x == '-'))

pLdapOid :: CharParser st String
pLdapOid = do
   num <- many1 (digit)
   rest <- many (do { string "."; n <- many1 (digit); return $ "."++n})
   return $ num ++ (concat $ rest)

-- | Basic Tokens
pSPACE :: CharParser st Char 
pSPACE = space

pFILL :: CharParser st ()
pFILL = spaces

pSEP = do
   try (char '\r' >> char '\n')
   <|> (char '\n')
