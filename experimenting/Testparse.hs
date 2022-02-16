module Main where 

import Language.Haskell.Exts

main ::  IO ()
main = do
  let file = "experimenting/tests/THExample.hs"
  res <- parseFile file
  -- putStrLn $ show res    -- print the annotated tree
  case res of           -- print tree without annotations
        f@ParseFailed{} -> print f
        ParseOk ast -> putStr $ show $ removeSrcSpanInfo ast
  --let ast' = transform ast
  prettyPrinterTest res


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


transform :: ParseResult (Module SrcSpanInfo) -> ParseResult (Module SrcSpanInfo)
transform f@ParseFailed{} = f
transform (ParseOk ast) = ParseOk $ transform' ast

transform' :: Module SrcSpanInfo -> Module SrcSpanInfo
transform' (Module srcinfo Nothing p i c) = 
  Module srcinfo 
    (Just 
      (ModuleHead srcinfo 
        (ModuleName srcinfo (extractFileName srcinfo)
        ) Nothing Nothing
      )
    ) p i c 
    where extractFileName s = case srcInfoSpan s of
            SrcSpan name _ _ _ _ -> removeExt name
          removeExt (n:".hs") = [n]
          removeExt (n:ns) = n : removeExt ns
           

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

