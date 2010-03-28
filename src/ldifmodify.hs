{-# LANGUAGE DeriveDataTypeable #-}
-- | Apply LDAP operations within LDIF on another LDIF.
-- | Without schema related verification like syntax, cardinality etc.
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF
import System.Console.CmdArgs

data LdifModify = LdifModify { baseFile :: FilePath
                             , modFiles  :: [FilePath]
                             , outFile  :: FilePath } deriving (Show, Data, Typeable)

defaultCfg = mode $ LdifModify { baseFile = def &= typFile & flag "f" & text "Base LDIF File"
                               , modFiles = def &= args & typ "LDIF Files for applying"
                               , outFile = def &= typFile & flag "o" & text "Output LDIF File" }

main = do
  cfg <- cmdArgs "LDIF Modify. Apply LDAP operations from LDIF to LDIF" [defaultCfg]
  baseLDIF <- safeParseLDIFFile (baseFile cfg)
  modLDIFs <- mapM (safeParseLDIFFile) (modFiles cfg)
  let outLDIF = foldr (applyLDIF) baseLDIF modLDIFs
  writeFile (outFile cfg) (ldif2str outLDIF)
  putStrLn $ (outFile cfg) ++ " written."

safeParseLDIFFile :: FilePath -> IO LDIF
safeParseLDIFFile name = liftM (either (\e -> error $ "Can not parse: "++(show e)) (id)) (parseLDIFFile name)
