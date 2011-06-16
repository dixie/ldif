-- | Make Change LDIF based on two Content LDIFs
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

main = do
  args <- getArgs
  xs <- B.readFile  (args !! 0)
  print $ B.length xs
  let ml1 = parseLDIFStr xs
  case ml1 of 
       Right l1 -> do
             print l1
             putStrLn "========================================================"
             B.putStrLn $ ldif2str l1
       Left err -> print err
