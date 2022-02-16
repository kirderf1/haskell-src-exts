module Main where 

import Language.Haskell.Exts
import Transform

main ::  IO ()
main = do
  -- let file = "experimenting/tests/THExample.hs"
  let file = "experimenting/tests/Example.hs"
  res <- parseFile file
  -- putStrLn $ show res    -- print the annotated tree
  case res of           -- print tree without annotations
        f@ParseFailed{} -> print f
        ParseOk ast -> putStrLn $ show $ removeSrcSpanInfo ast
  prettyPrinterTest res
  let ast' = transform res
  case ast' of           -- print tree without annotations
    f@ParseFailed{} -> print f
    ParseOk ast'' -> putStrLn $ show $ removeSrcSpanInfo ast''
  -- putStrLn $ show ast'
  prettyPrinterTest ast'



prettyPrinterTest :: ParseResult (Module SrcSpanInfo) -> IO () 
prettyPrinterTest ast = do
  let result = case ast of
        f@ParseFailed{} -> show f
        ParseOk ast -> prettyPrint ast
  putStrLn $ result 

parsetest :: FilePath -> IO ()
parsetest s = do
  ast <- parseFile s
  putStrLn $ show ast

parsePrettyPrinterTest :: FilePath -> IO () 
parsePrettyPrinterTest file = do
  ast <- parseFile file
  let
    result =
      case ast of
        f@ParseFailed{} -> show f
        ParseOk ast' -> prettyPrint ast'
  putStrLn $ result 
           

removeSrcSpanInfo :: Module SrcSpanInfo -> Module ()
removeSrcSpanInfo = fmap simplify
  where
    simplify :: SrcSpanInfo -> ()
    simplify _ = ()

  --     data SrcSpanInfo = SrcSpanInfo
  --     { srcInfoSpan    :: SrcSpan
  -- --    , explLayout     :: Bool
  --     , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
  --     }

