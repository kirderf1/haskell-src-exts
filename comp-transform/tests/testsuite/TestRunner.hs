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
    --defaultMain $ runTests (goodSources, progressSources, badSources)
    defaultMain $ testGood $ goodDir ++ "/PieceConst.hs"
  
  
 
type TestSuite = ([FilePath], [FilePath], [FilePath])
  
testsuite :: FilePath
testsuite = "comp-transform/tests/testsuite/"
  
goodDir :: FilePath
goodDir = testsuite ++ "good"

progressDir :: FilePath
progressDir = testsuite ++ "progress"

badDir :: FilePath
badDir = testsuite ++ "bad"
  
getTestFiles :: MonadIO m => FilePath -> m [FilePath]
getTestFiles dir = liftIO $ findByExtension [".hs"] dir
  
{-
runTests :: TestSuite -> IO ()
runTests (goodSources, progressSources, badSources) = do
    good <- mapM testGood goodSources
    print good
    -}  
    
testGood :: FilePath -> TestTree
testGood file = 
    -- expectedOutput <- readFile (file ++ ".out")
    let golden = file <.> "golden"
        out = file <.> "out"
        run = do
            parseResult <- parseFile file
            case parseResult of 
                f@ParseFailed{} -> error $ show f
                ParseOk ast -> do
                    case runExcept (transform $ void ast) of
                        Left msg -> error msg
                        Right ast' -> do let result = prettyPrint ast'
                                         writeBinaryFile out $ result ++ "\n"
    in    
            -- transformResult <- runTransform ast
            -- putStrLn transformResult
            -- putStrLn expectedOutput
    goldenVsFile (takeBaseName file) golden out run
           -- return $ goldenVsString (takeBaseName file) golden (runTransform ast)

        
    
  
