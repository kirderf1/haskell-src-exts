module Main where 

import Language.Haskell.Exts
import Transform

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
      prettyPrinterTest ast
      let res = transform ast
      case res of           -- print tree without annotations
            f@ParseFailed{} -> print f
            ParseOk ast' -> do
              putStrLn "AST structure after:"
              showModule ast'

showModule :: Module SrcSpanInfo -> IO()
showModule = putStrLn . show . removeSrcSpanInfo

prettyPrinterTest :: Module SrcSpanInfo -> IO () 
prettyPrinterTest ast = putStrLn $ prettyPrint ast

removeSrcSpanInfo :: Module SrcSpanInfo -> Module ()
removeSrcSpanInfo = fmap $ const ()

  --     data SrcSpanInfo = SrcSpanInfo
  --     { srcInfoSpan    :: SrcSpan
  -- --    , explLayout     :: Bool
  --     , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
  --     }

