module Main where

import Language.Haskell.Exts
import Transform

import Control.Monad.Trans
import Test.Tasty.Golden
import Test.Tasty
import Control.Monad.Except
import System.FilePath
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)


main :: IO ()
main = do
    tests <- createTree `mapM` groups
    defaultMain $ testGroup "Tests" tests
  
 
type TestSuite = ([FilePath], [FilePath], [FilePath])
  
testsuite :: FilePath
testsuite = "comp-transform/tests/testsuite/"

groups :: [FilePath]
groups = [ "good", "progress", "bad"]

goldenDir :: FilePath
goldenDir = "golden"

getGoldenPath :: FilePath -> FilePath
getGoldenPath file = replaceDirectory file (takeDirectory file </> goldenDir) <.> "golden"

outputDir :: FilePath
outputDir = "output"

getOutputPath :: FilePath -> FilePath
getOutputPath file = replaceDirectory file (takeDirectory file </> outputDir) <.> "out"

  
getTestFiles :: MonadIO m => FilePath -> m [FilePath]
getTestFiles dir = liftIO $ do
    exists <- doesDirectoryExist dir
    if exists
      then findByExtension [".hs"] dir
      else return []

createTree :: FilePath -> IO (TestTree)
createTree group = testGolden group <$> getTestFiles (testsuite ++ group)    

testGolden :: String -> [FilePath] -> TestTree
testGolden group files = testGroup group $ do
    file <- files
    let golden = getGoldenPath file 
        out = getOutputPath file
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
