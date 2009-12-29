module Text.LDIF.Proc (
	ldifdiff 
)
where
import Text.LDIF.Types

-- | Create Change LDIF between to LDIF contents 
ldifdiff :: LDIF -> LDIF -> LDIF
ldifdiff l1 l2 = l1
