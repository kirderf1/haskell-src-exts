module Main where 

import Language.Haskell.Exts
import Transform
import Control.Monad.Except
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of 
    ("debug":[file]) -> do
        res <- parseFile file
        case res of 
            f@ParseFailed{} -> print f
            ParseOk ast -> runTests ast True
    [file] -> do
  -- let file = "experimenting/tests/Common.hs"
        res <- parseFile file
        case res of   
            f@ParseFailed{} -> print f
            ParseOk ast -> runTests ast False
    _ -> putStrLn "Wrong number of arguments"

  where
    runTests :: Module SrcSpanInfo -> Bool -> IO ()
    runTests ast debug = do
      when debug
        $ do putStrLn "AST structure before:"
             showModule ast
             putStrLn "Pretty-print before:"
             putStrLn $ prettyPrint ast
      case runExcept (transform $ void ast) of
         Left msg -> putStrLn msg
         Right ast' -> if debug
                          then do putStrLn "AST structure after:"
                                  showModule ast'
                                  putStrLn "Pretty-print after:"
                                  putStrLn $ prettyPrint ast'
                          else putStrLn $ prettyPrint ast'

showModule :: Module l -> IO ()
showModule = print . void

  --     data SrcSpanInfo = SrcSpanInfo
  --     { srcInfoSpan    :: SrcSpan
  -- --    , explLayout     :: Bool
  --     , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
  --     }

