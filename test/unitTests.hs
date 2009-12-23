import Text.LDIF
import Test.HUnit
import Data.Either
import Data.List
import Directory
import System.FilePath
import Control.Monad (liftM)

validLDIFDir = "valid"

main = do
    validLDIFFiles <- liftM (filter (\x -> isSuffixOf ".ldif" x)) $ getDirectoryContents validLDIFDir
    runTestTT (tests validLDIFFiles)

testCasesParseValid validFiles = map (\x -> TestCase (assertParsedOK x)) (map (validLDIFDir </>) validFiles)

assertParsedOK filename = do
     ret <- parseLDIFFile filename 
     either (\e -> assertFailure (show e)) (\ldif -> assertParsedType filename ldif) ret

assertParsedType name ldif | (isSuffixOf ".modify.ldif" name) = assertTypeChanges name ldif
                           | (isSuffixOf ".content.ldif" name) = assertTypeContent name ldif
                           | otherwise = assertFailure $ "Unexpected filename: (not .modify.ldif or .content.ldif " ++ name

assertTypeContent n l@(LDIFContent _ _) = assertBool "Valid Content Type" True >> (putStrLn $ "\n\n" ++ n ++ "\n\n" ++ (show l))
assertTypeContent n x = assertFailure $ n ++ " is not type of LDIFContent"

assertTypeChanges n l@(LDIFChanges _ _) = assertBool "Valid Changes Type" True >> (putStrLn $ "\n\n" ++ n ++ "\n\n" ++ (show l))
assertTypeChanges n x = assertFailure $ n ++ " is not type of LDIFChanges"

tests validFiles = TestList (testCasesParseValid validFiles)
  
