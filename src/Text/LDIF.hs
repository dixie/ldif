module Text.LDIF (
	parseLDIFStr,
	parseLDIFFile,
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
import Data.List (isPrefixOf)

type Attribute = String
type Value = String
type AttrValue = (Attribute, Value)
type DN = String

-- | Represents LDIF structure, it can be either simply LDIF data dump or
-- | changes LDIF with LDAP operations 
data LDIF = LDIFContent { lcVersion :: Maybe String, lcEntries :: [Record] }
          | LDIFChanges { lcVersion :: Maybe String, lcEntries :: [Record] } deriving Show

-- | Represents one record or entry within LDIF file with DN and content
data Record = AttrValRecord { recDN :: DN, recAttrVals :: [AttrValue] }  
	    | ChangeRecord  { recDN :: DN, recOp :: Change } deriving Show

-- | Represents one LDAP operation within changes LDIF
data Change = ChangeAdd     { chAttrVals :: [AttrValue] }
            | ChangeDelete 
            | ChangeModify  { chMods :: [Modify] }
            | ChangeModDN  deriving Show

-- | Represents ChangeModify operations upon one entry within given DN
data Modify = ModAdd     { modAttr :: Attribute, modAttrVals :: [AttrValue] }
            | ModDelete  { modAttr :: Attribute, modAttrVals :: [AttrValue] }
            | ModReplace { modAttr :: Attribute, modAttrVals :: [AttrValue] } deriving Show

-- | Parse string as LDIF content and return LDIF or ParseError
parseLDIFStr :: String -> Either ParseError LDIF
parseLDIFStr = parse pLdif "(param)" . preproc 

-- | Read and parse provided file and return LDIF or ParseError
parseLDIFFile :: String -> IO (Either ParseError LDIF)
parseLDIFFile name = do
	input <- readFile name
	return $ parse pLdif name (preproc input)

-- | Preprocessing for concat wrapped lines and remove comment lines
preproc :: String -> String
preproc = unwrap . stripComments

-- | Remove Comment Lines
stripComments :: String -> String
stripComments input = unlines $ filter (not . isPrefixOf "#") $ lines input

-- | Unwrap lines, lines with space at begin is continue of previous line 
unwrap :: String -> String
unwrap input = unlines $ preprocLines $ lines input
   where 
    preprocLines xs = unbox $ foldl (preprocLine) ([],Nothing) xs
    preprocLine (ys,r) []                 = (addLineMaybe ys r,Just []) 
    preprocLine (ys,r) (l:lx) | l == ' '  = (ys,concatLineMaybe r lx)
                              | otherwise = (addLineMaybe ys r, Just $ l:lx)
    concatLineMaybe Nothing  x = Just x
    concatLineMaybe (Just y) x = Just (y++x)
    addLineMaybe xs Nothing  = xs
    addLineMaybe xs (Just x) = xs++[x]
    unbox (ys,Nothing) = ys
    unbox (ys,Just x)  = ys++[x]

-- | Parsec ldif parser
pLdif :: CharParser st LDIF
pLdif = try pLdifChanges <|> pLdifContent

pLdifChanges :: CharParser st LDIF
pLdifChanges = do
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy1 pChangeRec pSEPs
    return $ LDIFChanges ver recs

pLdifContent :: CharParser st LDIF
pLdifContent = do
    ver <- optionMaybe pVersionSpec
    recs <- sepEndBy1 pAttrValRec pSEPs
    return $ LDIFContent ver recs

pAttrValRec ::  CharParser st Record
pAttrValRec = do
    dn <- pDNSpec
    pSEP
    attrVals <- sepEndBy1 pAttrValSpec pSEP
    return $ AttrValRecord dn attrVals

pChangeRec :: CharParser st Record
pChangeRec = try pChangeAdd
         <|> try pChangeDel
         <|> try pChangeMod
         <|> pChangeModDN

pChangeAdd :: CharParser st Record
pChangeAdd = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
    string "add"
    pSEP
    vals <- sepEndBy1 pAttrValSpec pSEP
    return $ ChangeRecord dn (ChangeAdd vals)

pChangeDel :: CharParser st Record
pChangeDel = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
    string "delete"
    pSEP
    return $ ChangeRecord dn ChangeDelete

pChangeMod :: CharParser st Record
pChangeMod = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
    string "modify"
    pSEP
    mods <- sepEndBy1 pModSpec (char '-' >> pSEP)
    return $ ChangeRecord dn (ChangeModify mods)

pChangeModDN :: CharParser st Record
pChangeModDN = do
    dn <- pDNSpec
    pSEP
    string "changetype:"
    pFILL
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
    return $ ChangeRecord dn ChangeModDN

pRDN :: CharParser st String
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
   many1 digit

pModSpec :: CharParser st Modify
pModSpec = do
   modType <- pModType
   pFILL
   att <- pAttributeDescription 
   pSEP 
   vals <- sepEndBy pAttrValSpec pSEP
   return $ mkMod modType att vals

mkMod :: String -> String -> [AttrValue] -> Modify
mkMod modType att vals | modType == "add:" = ModAdd att vals
                       | modType == "delete:" = ModDelete att vals
                       | modType == "replace:" = ModReplace att vals
                       | otherwise = error $ "unexpected mod:" ++ modType 
                         -- error can not be reached because pModType

pModType :: CharParser st String
pModType = try (string "add:")
       <|> try (string "delete:")
       <|> string "replace:"

pAttributeDescription :: CharParser st String
pAttributeDescription = pAttributeType

pAttributeType :: CharParser st String
pAttributeType = try pLdapOid
             <|> (do { l <- letter; o <- pAttrTypeChars; return $ l:o } )

pAttrValSpec :: CharParser st AttrValue
pAttrValSpec = do
   name <- pAttributeDescription
   val  <- pValueSpec
   return (name, val)

pValueSpec :: CharParser st Value
pValueSpec = try (char ':' >> char ':' >> pFILL >> pBase64String)
         <|> try (char ':' >> pFILL >> pSafeString) 
         <|> (char ':' >> char '<' >> pFILL >> pURL)

pURL :: CharParser st String
pURL = pSafeString

pSafeString :: CharParser st String
pSafeString = do
   c <- noneOf "\n\r :<"
   r <- many (noneOf "\n\r")
   return $ c:r
 
pBase64String :: CharParser st String
pBase64String = pSafeString

pAttrTypeChars :: CharParser st String
pAttrTypeChars = many (satisfy (\x -> isAlphaNum x || x == '-'))

pLdapOid :: CharParser st String
pLdapOid = do
   num <- many1 digit
   rest <- many (do { string "."; n <- many1 digit; return $ '.':n})
   return $ num ++ concat rest

pFILL :: CharParser st ()
pFILL = spaces

pSEP :: CharParser st ()
pSEP = try (char '\r' >> char '\n' >> return () )
   <|> (char '\n' >> return () )

pSEPs :: CharParser st ()
pSEPs = many pSEP >> return ()

