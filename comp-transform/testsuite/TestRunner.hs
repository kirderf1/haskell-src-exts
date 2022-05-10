module Main where

import Language.Haskell.Exts
import Transform

import Test.Tasty.Golden
import Test.Tasty
import Control.Monad.Except
import System.FilePath
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import System.Process     (readProcessWithExitCode)
import System.Exit


main :: IO ()
main = do
    transformTests <- createTransformTree `mapM` groups
    compileTests <- createCompileTree `mapM` ["good"]
    defaultMain $ testGroup "Tests" 
        [(testGroup "Transform tests") transformTests, 
         (testGroup "Compile tests") compileTests]          
            
      
 
type TestSuite = ([FilePath], [FilePath], [FilePath])
  
testsuite :: FilePath
testsuite = "comp-transform" </> "testsuite"

groups :: [FilePath]
groups = ["good", "bad"]
  
getTestFiles :: FilePath -> IO [FilePath]
getTestFiles mainDir = do
    exists <- doesDirectoryExist mainDir
    if exists
      then do
        dirs <- getDirectories mainDir
        return $ map (\dir -> dir </> takeBaseName dir <.> "hs") dirs
      else return []

createTransformTree :: FilePath -> IO (TestTree)
createTransformTree group = testGolden group "Transform" runTransformTest <$> getTestFiles (testsuite </> group)

createCompileTree :: FilePath -> IO (TestTree)
createCompileTree group = testGolden group "Compile" runTransformAndCompileTest <$> getTestFiles (testsuite </> group)

testGolden :: String -> String -> (FilePath -> FilePath -> IO ()) -> [FilePath] -> TestTree
testGolden group test run files = testGroup group $ do
    file <- files
    let golden = dropExtension file ++ test <.> "golden"
        out = dropExtension file ++ test <.> "out"
    return $ goldenVsFile (takeBaseName file) golden out (run file out)

runTransformTest :: FilePath -> FilePath -> IO ()
runTransformTest file out = do
    parseResult <- parseFile file
    case parseResult of 
        f@ParseFailed{} -> writeFileAndCreateDirectory out $ show f ++ "\n"
        ParseOk ast -> do
            case runExcept (transform $ void ast) of
                Left msg ->  writeFileAndCreateDirectory out $ msg ++ "\n"
                Right ast' -> do let result = prettyPrint ast'
                                 writeFileAndCreateDirectory out $ result ++ "\n"

runTransformAndCompileTest :: FilePath -> FilePath -> IO ()
runTransformAndCompileTest file out = do
    let dir = takeDirectory file </> "build"
    let transformOutput = dir </> "output" <.> "hs"
    runTransformTest file transformOutput
    runCompileTest dir transformOutput out
    removeDirectoryRecursive dir
                                 
runCompileTest :: FilePath -> FilePath -> FilePath -> IO ()
runCompileTest dir file outFile = do
    -- let runFile = dropExtension file
    createDirectoryIfMissing True dir
    (exit,_out,_err) <- readProcessWithExitCode "ghc" ["-outputdir", dir, "-o", dir </> "output", file] []
    case exit of
         ExitSuccess -> writeFileAndCreateDirectory outFile $  "OK \n"
         ExitFailure _ -> do 
            (exit2,_out2,err2) <- readProcessWithExitCode "ghc" ["-outputdir", dir, file] []
            case exit2 of
                ExitFailure _ -> writeFileAndCreateDirectory outFile err2
                ExitSuccess -> writeFileAndCreateDirectory outFile $  "OK \n"

writeFileAndCreateDirectory :: FilePath -> String -> IO ()
writeFileAndCreateDirectory file text = do
    createDirectoryIfMissing True $ takeDirectory file
    writeBinaryFile file text

    
getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = listDirectory filePath
                      >>= return . map (filePath </>)
                      >>= filterM doesDirectoryExist
