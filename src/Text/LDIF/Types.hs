module Text.LDIF.Types (
 	LDIF(..),   
        Record(..),
        Change(..),
        Modify(..), 
        DN(..), 
        Attribute, Value, AttrValue
)
where
import Data.Char

type Attribute = String
type Value = String
type AttrValue = (Attribute, Value)

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

-- | Represents Distinguished Name (DN)
data DN = DN { dnAttrVals :: [AttrValue] } deriving Show


