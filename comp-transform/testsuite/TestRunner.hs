module Main where

import Language.Haskell.Exts
import Transform

import Test.Tasty.Golden
import Test.Tasty
import Control.Monad.Except
import System.FilePath
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, listDirectory)
import System.Process     (callProcess, readProcessWithExitCode)
import System.Exit


main :: IO ()
main = do
    transformTests <- createTransformTree `mapM` groups
    compileTests <- createCompileTree `mapM` ["good", "unclear"]
    defaultMain $ testGroup "Tests" 
        [(testGroup "Transform tests") transformTests, 
         (testGroup "Compile tests") compileTests]          
            
      
 
type TestSuite = ([FilePath], [FilePath], [FilePath])
  
testsuite :: FilePath
testsuite = "comp-transform/testsuite/"

groups :: [FilePath]
groups = ["good", "unclear", "bad"]
  
getTestFiles :: FilePath -> IO [FilePath]
getTestFiles dir = do
    exists <- doesDirectoryExist dir
    if exists
      then do
        dirs <- getDirectories dir 
        liftM concat $ mapM (findByExtension [".hs"])  dirs
      else return []

createTransformTree :: FilePath -> IO (TestTree)
createTransformTree group = testGolden group "Transform" runTransformTest <$> getTestFiles (testsuite ++ group)    

createCompileTree :: FilePath -> IO (TestTree)
createCompileTree group = testGolden group "Compile" runTransformAndCompileTest <$> getTestFiles (testsuite ++ group) 

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
    let transformed = dropExtension file ++ "Transform" <.> "hs"
    runTransformTest file transformed
    runCompileTest transformed out
                                 
runCompileTest :: FilePath -> FilePath -> IO ()
runCompileTest file outFile = do
    -- let runFile = dropExtension file
    let build = takeDirectory file </> "build"
    callProcess "mkdir" ["-p", build]
    (exit,_out,_err) <- readProcessWithExitCode "ghc" ["-outputdir", build, "-o", build </> "output", file] []
    case exit of
         ExitSuccess -> do writeFileAndCreateDirectory outFile $  "OK \n"
                           callProcess "rm" ["-r", file, build]
         ExitFailure _ -> do 
            (exit2,_out2,err2) <- readProcessWithExitCode "ghc" ["-outputdir", build, file] []
            case exit2 of
                ExitFailure _ -> do writeFileAndCreateDirectory outFile err2
                                    callProcess "rm" ["-r", file, build]
                ExitSuccess -> do writeFileAndCreateDirectory outFile $  "OK \n"
                                  callProcess "rm" ["-r", file, build]
        {-    do
             (exit2,out2,err2) <- readProcessWithExitCode runFile [] []
             case exit2 of
                  ExitFailure _ -> writeFileAndCreateDirectory outFile err >> callProcess "rm" [file]
                  ExitSuccess -> do writeFileAndCreateDirectory outFile $  "OK \n"
                                    callProcess "rm" [file]
                                        -}
                                 
                                 

writeFileAndCreateDirectory :: FilePath -> String -> IO ()
writeFileAndCreateDirectory file text = do
    createDirectoryIfMissing True $ takeDirectory file
    writeBinaryFile file text

    
getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = listDirectory filePath
                      >>= filterM (doesDirectoryExist . (filePath </>))
                      >>= return . map (filePath </>)
