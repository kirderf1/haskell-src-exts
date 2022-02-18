module Main where 

import Language.Haskell.Exts
import Transform
import Control.Monad.Except


main ::  IO ()
main = do
  let file = "experimenting/tests/Example.hs"
  res <- parseFile file
  case res of           -- print tree without annotations
        f@ParseFailed{} -> print f
        ParseOk ast -> runTests ast
  where
    runTests :: Module SrcSpanInfo -> IO ()
    runTests ast = do
      putStrLn "AST structure before:"
      showModule ast
      putStrLn "Pretty-print before:"
      putStrLn $ prettyPrint ast
      case runExcept (transform ast) of
           Left msg -> putStrLn msg
           Right ast' -> do putStrLn "AST structure after:"
                            showModule ast'
                            putStrLn "Pretty-print after:"
                            putStrLn $ prettyPrint ast'

showModule :: Module SrcSpanInfo -> IO()
showModule = putStrLn . show . removeSrcSpanInfo

removeSrcSpanInfo :: Module SrcSpanInfo -> Module ()
removeSrcSpanInfo = fmap $ const ()

  --     data SrcSpanInfo = SrcSpanInfo
  --     { srcInfoSpan    :: SrcSpan
  -- --    , explLayout     :: Bool
  --     , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
  --     }

