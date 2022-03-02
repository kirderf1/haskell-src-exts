module Main where 

import Language.Haskell.Exts
import Transform
import Control.Monad.Except
import System.Environment


main :: IO ()
main = do
  (debug, file) <- parseArgs <$> getArgs
  res <- parseFile file
  case res of 
      f@ParseFailed{} -> error $ show f
      ParseOk ast -> runTests ast debug
  where
    runTests :: Module SrcSpanInfo -> Bool -> IO ()
    runTests ast debug = do
      when debug
        $ do putStrLn "AST structure before:"
             showModule ast
             putStrLn "Pretty-print before:"
             putStrLn $ prettyPrint ast
      case runExcept (transform $ void ast) of
         Left msg -> error msg
         Right ast' -> if debug
                          then do putStrLn "AST structure after:"
                                  showModule ast'
                                  putStrLn "Pretty-print after:"
                                  putStrLn $ prettyPrint ast'
                          else putStrLn $ prettyPrint ast'

parseArgs :: [String] -> (Bool, FilePath)
parseArgs ("debug":[file]) = (True, file)
parseArgs [file] = (False, file)
parseArgs _ = error "Wrong number of arguments"

showModule :: Module l -> IO ()
showModule = print . void

  --     data SrcSpanInfo = SrcSpanInfo
  --     { srcInfoSpan    :: SrcSpan
  -- --    , explLayout     :: Bool
  --     , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
  --     }

