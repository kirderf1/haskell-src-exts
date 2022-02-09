module Testparse where 

import Language.Haskell.Exts

main :: FilePath -> IO ()
main file = do
  ast <- parseFile file
  let ast' = transform ast
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
        ParseOk ast -> prettyPrint ast
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
           

