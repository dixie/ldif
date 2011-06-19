-- | Make Change LDIF based on two Content LDIFs
import Data.List
import Data.Either
import Data.Maybe
import Control.Monad 
import System.FilePath 
import System.Environment
import Text.LDIF
import qualified Data.ByteString.Char8 as BC

main = do
  args <- getArgs
  xs <- BC.readFile  (args !! 0)
  print $ BC.length xs
  let ml1 = parseLDIFStr xs
  case ml1 of 
       Right l1 -> do
             BC.putStrLn $ ldif2str l1
       Left err -> print err
