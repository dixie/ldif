-- | LDIF serializers
module Text.LDIF.Printer (
	ldif2Str,
	change2Str,
	content2Str
)
where
import Text.LDIF.Types

-- | Serialize LDIF in LDIF Format
ldif2Str :: LDIF -> String
ldif2Str x = (show x)

-- | Serialize Content Record in LDIF Format
content2Str :: ContentRecord -> String
content2Str x = (show x)

-- | Serialize Change Record in LDIF Format
change2Str :: ChangeRecord -> String
change2Str x = (show x)
