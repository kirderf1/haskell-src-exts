module Main where

import Language.Haskell.Exts
import Transform

import Control.Monad.Trans
import Test.Tasty.Golden
import Test.Tasty
import Control.Monad.Except
import System.FilePath


main :: IO ()
main = do
    goodSources <- getTestFiles goodDir
    progressSources <- getTestFiles progressDir
    badSources <- getTestFiles badDir
    defaultMain $ testGroup "Tests" $
        [ testGolden goodSources,
          testGolden progressSources,
          testGolden badSources
        ]  
  
 
type TestSuite = ([FilePath], [FilePath], [FilePath])
  
testsuite :: FilePath
testsuite = "comp-transform/tests/testsuite/"
  
goodDir :: FilePath
goodDir = testsuite ++ "good"

progressDir :: FilePath
progressDir = testsuite ++ "progress"

badDir :: FilePath
badDir = testsuite ++ "bad"

goldenDir :: FilePath
goldenDir = "golden"

getGolden :: FilePath -> FilePath
getGolden file = replaceDirectory file (takeDirectory file </> goldenDir) <.> "golden"

outputDir :: FilePath
outputDir = "output"

getOutput :: FilePath -> FilePath
getOutput file = replaceDirectory file (takeDirectory file </> outputDir) <.> "out"

  
getTestFiles :: MonadIO m => FilePath -> m [FilePath]
getTestFiles dir = liftIO $ findByExtension [".hs"] dir
  
    
testGolden :: [FilePath] -> TestTree
testGolden files = testGroup "Transform tests" $ do
    file <- files
    let golden = getGolden file 
        out = getOutput file
        run = do
            parseResult <- parseFile file
            case parseResult of 
                f@ParseFailed{} -> writeBinaryFile out $ show f ++ "\n"
                ParseOk ast -> do
                    case runExcept (transform $ void ast) of
                        Left msg ->  writeBinaryFile out $ msg ++ "\n"
                        Right ast' -> do let result = prettyPrint ast'
                                         writeBinaryFile out $ result ++ "\n"
    return $ goldenVsFile (takeBaseName file) golden out run


    
  
