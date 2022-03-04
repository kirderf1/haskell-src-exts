module Main where

import Language.Haskell.Exts
import Transform

import Test.Tasty.Golden
import Test.Tasty
import Control.Monad.Except
import System.FilePath
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, listDirectory)


main :: IO ()
main = do
    tests <- createTree `mapM` groups
    defaultMain $ testGroup "Tests" tests
  
 
type TestSuite = ([FilePath], [FilePath], [FilePath])
  
testsuite :: FilePath
testsuite = "comp-transform/testsuite/"

groups :: [FilePath]
groups = [ "good", "unclear", "bad"]
  
getTestFiles :: FilePath -> IO [FilePath]
getTestFiles dir = do
    exists <- doesDirectoryExist dir
    if exists
      then do
        dirs <- getDirectories dir 
        liftM concat $ mapM (findByExtension [".hs"])  dirs
      else return []

createTree :: FilePath -> IO (TestTree)
createTree group = testGolden group <$> getTestFiles (testsuite ++ group)    

testGolden :: String -> [FilePath] -> TestTree
testGolden group files = testGroup group $ do
    file <- files
    let golden = file <.> "golden"
        out = file <.> "out"
    return $ goldenVsFile (takeBaseName file) golden out (runTest file out)

runTest :: FilePath -> FilePath -> IO ()
runTest file out = do
    parseResult <- parseFile file
    case parseResult of 
        f@ParseFailed{} -> writeFileAndCreateDirectory out $ show f ++ "\n"
        ParseOk ast -> do
            case runExcept (transform $ void ast) of
                Left msg ->  writeFileAndCreateDirectory out $ msg ++ "\n"
                Right ast' -> do let result = prettyPrint ast'
                                 writeFileAndCreateDirectory out $ result ++ "\n"

writeFileAndCreateDirectory :: FilePath -> String -> IO ()
writeFileAndCreateDirectory file text = do
    createDirectoryIfMissing True $ takeDirectory file
    writeBinaryFile file text

    
getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = listDirectory filePath
                      >>= filterM (doesDirectoryExist . (filePath </>))
                      >>= return . map (filePath </>)
