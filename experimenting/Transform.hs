module Transform where

import Language.Haskell.Exts

import Types

import           Data.Map   (Map)
import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.Except

-- | Map of category names to pieces
type Sig = Map String (Set String)

-- | Transform monad containing signature of categories and handles error messages as Strings.
type Transform = ReaderT Sig (Except String)

transform :: Module l -> Except String (Module l)
transform m@(Module _l _mhead _pragmas _imports decls) = do
    sig <- buildSig decls
    runReaderT (transformModule m) sig 
transform _xml = throwError "transform not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a module to remove syntax for composable types if the pragma is present
transformModule :: Module l -> Transform (Module l)
transformModule m@(Module l mhead pragmas imports decls) =
    if pragmasContain "ComposableTypes" pragmas
        then do
            let pragmas' = modifyPragmas l pragmas
                imports' = modifyImports l imports
            
            decls' <- liftM concat (mapM transformDecl decls)
            mapType transformType (Module l mhead pragmas' imports' decls')
        else return m
transformModule _xml = throwError "transformModule not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a top level declaration to one or more new declarations
transformDecl :: Decl l -> Transform [Decl l]
transformDecl (PieceDecl l category headName cons derives) = 
    let nameL = ann headName
        parname = getParName nameL
        cspar = map (parametConstructor parname category) cons
        aders = head $ map ann derives
        functorDerive = deriveFunctor aders
        in
    return ((DataDecl 
        l 
        (DataType l)
        Nothing 
        (DHApp nameL (DHead nameL headName) (UnkindedVar nameL parname))
        cspar
        (functorDerive : derives)
        )
        : [deriveTHPiece headName])

transformDecl d = return [d]

-- | Transform a type
transformType :: Type l -> Transform (Type l)
transformType (TyComp l category types) = do
    -- check if piece constructors are in category
    cats <- ask
    cat <- lift $ catName category
    case Map.lookup cat cats of
        Nothing -> throwError "transformType: Trying to form type of unknown category"
        Just pieces -> do 
            typeNames <- mapM (lift . catName) types
            lift $ checkInCategory pieces typeNames
            coprodtype <- coprod types
            return $ TyApp l (TyCon l (UnQual l (Ident l "Term"))) (TyParen l coprodtype)
      where catName :: QName l -> Except String String
            catName (Qual _l _moduleName (Ident _l1 c)) = return c
            catName (UnQual _l (Ident _l1 c)) = return c
            catName _ = throwError "transformType: unexpected type of category name"
            -- TODO: special QName?
transformType t = return t

{- | Parametrize a piece constructor to have a parametrized variable as recursive 
    parameter instead of the name of the category it belongs to.
-}
parametConstructor :: Name l -> Name l -> QualConDecl l -> QualConDecl l
parametConstructor parname category (QualConDecl l0 v c (ConDecl l1 cname types)) = 
    QualConDecl l0 v c (ConDecl l1 cname (map (parametType parname) types))
    where 
        -- TODO: Could there be other ways to form this construct?
        parametType pname (TyCon tl (UnQual t2 recu)) = 
            if recu `match` category
                then TyCon tl (UnQual t2 pname)
                else TyCon tl (UnQual t2 recu)
        parametType _ t = t
        match (Ident _ n1) (Ident _ n2) = n1 == n2
        match _ _ = False
parametConstructor _ _ c = c 


-- TODO: Add deriving functor in tuple of one derive, not in list. 
-- (If we want user to be able to add deriving clauses)
-- Gives this error message for multiple deriving clauses:
-- Illegal use of multiple, consecutive deriving clauses
-- Use DerivingStrategies to allow this

-- | Create a Deriving functor for a given data type
deriveFunctor :: l -> Deriving l 
deriveFunctor l =
  Deriving l Nothing
    [IRule l Nothing 
      Nothing
      (IHCon l (UnQual l (Ident l "Functor")))]



-- | Get a name for the parametrized variable.            
-- TODO: Figure out how to handle unique names for parametrize variables
getParName :: l -> Name l 
getParName info = Ident info "a"


-- | Template Haskell derive for a piece
deriveTHPiece :: Name l -> Decl l 
deriveTHPiece pieceName = deriveTH pieceName ["makeTraversable", "makeFoldable", "makeEqF",
                                  "makeShowF", "smartConstructors", "smartAConstructors"]                                

-- | Template Haskell derive for a certain data type from a list of things to derive
deriveTH :: Name l -> [String] -> Decl l 
deriveTH targetName list = SpliceDecl l 
        (SpliceExp l 
            (ParenSplice l 
                (App l 
                    (App l 
                        (Var l
                            (UnQual l 
                                (Ident l "Data.Comp.Derive.derive")
                            )
                        ) 
                        (List l 
                            (map (deriveTHListElem l) list)
                        )
                    ) 
                    (List l 
                        [TypQuote l
                            (UnQual l
                                targetName
                            )
                        ]
                    )
                )
            )
        )
    where l = ann targetName


-- | Element for a thing to derive with Template Haskell
deriveTHListElem :: l -> String -> Exp l
deriveTHListElem l nam = Var l (UnQual l (Ident l ("Data.Comp.Derive." ++ nam)))


-- TODO: derive liftSum for the class for a function/algebra


-- TODO: possibly more pragmas?
-- | Modify a list of pragmas to remove ComposableTypes and add the ones needed for compdata
modifyPragmas :: l -> [ModulePragma l] -> [ModulePragma l]
modifyPragmas l ps =  concatMap (addPragma l (removeCompTypes ps)) 
                                ["DeriveFunctor","TemplateHaskell","TypeOperators"
                                ,"FlexibleContexts"] 
    where  
        addPragma :: l -> [ModulePragma l] -> String -> [ModulePragma l]
        addPragma l1 prs nam = if pragmasContain nam prs 
                                 then prs
                                 else (LanguagePragma l1 [Ident l1 nam]):prs
        removeCompTypes = filter (not . matchPragma "ComposableTypes")

-- | Check if the list of pragmas contain a certain one
pragmasContain :: String -> [ModulePragma l] -> Bool
pragmasContain nam = any (matchPragma nam)
        
-- | Check if a pragma match the given String
matchPragma :: String -> ModulePragma l -> Bool
matchPragma s (LanguagePragma _ [Ident _ nam]) = nam == s
matchPragma _ _ = False

-- | Form coproduct type from a list of pieces
coprod :: [QName l] -> Transform (Type l)
coprod [nam] = return $ TyCon l nam
    where l = ann nam
coprod (nam:ns) = do
    rest <- coprod ns
    return (TyInfix l (TyCon l nam)
                      (UnpromotedName l (Qual l (ModuleName l "Data.Comp") (Symbol l ":+:")))
                       rest)
    where l = ann nam
coprod _ = throwError "Trying to form coproduct of no arguments"

-- | Build signature, map of categories to their pieces
buildSig :: [Decl l] -> Except String Sig
buildSig [] = return Map.empty 
buildSig ((PieceDecl _l category headName _cons _derives):decls) = do
    sig <- buildSig decls 
    idCat <- nameID category
    idHead <- nameID headName
    case Map.lookup idCat sig of
        Just oldCons -> return $ Map.insert idCat (Set.insert idHead oldCons) sig
        Nothing -> return $ Map.insert idCat (Set.fromList [idHead]) sig
    where 
        nameID :: Name l -> Except String String
        nameID (Ident _ s) = return s
        nameID _ = throwError "buildSig: unexpected type of name"
    
buildSig (_:decls) = buildSig decls

-- | Modify a list of import declarations to add the ones needed for compdata
modifyImports :: l -> [ImportDecl l] -> [ImportDecl l]
modifyImports l is =  concatMap (addImport l is)
                                ["Data.Comp", "Data.Comp.Derive",
                                 "Data.Comp.Show ()", "Data.Comp.Equality ()"] 
    where  
        addImport :: l -> [ImportDecl l] -> String -> [ImportDecl l]
        addImport l1 is1 nam = if importsContain nam is1 
                                 then is1
                                 else (ImportDecl
                                 { importAnn = l1                     -- ^ annotation, used by parser for position of the @import@ keyword.
                                 , importModule = ModuleName l1 nam   -- ^ name of the module imported.
                                 , importQualified = True            -- ^ imported @qualified@?
                                 , importSrc = False                  -- ^ imported with @{-\# SOURCE \#-}@?
                                 , importSafe = False                 -- ^ Import @safe@?
                                 , importPkg = Nothing                -- ^ imported with explicit package name
                                 , importAs = Nothing                 -- ^ optional alias name in an @as@ clause.
                                 , importSpecs = Nothing              -- ^ optional list of import specifications.
                                 }):is1


-- | Check if the list of import declarations contain a certain one
importsContain :: String -> [ImportDecl l] -> Bool
importsContain nam = any (matchImport nam)
        
-- | Check if a import declaration match the given String
matchImport :: String -> ImportDecl l -> Bool
matchImport s (ImportDecl {importModule = ModuleName _ nam}) = nam == s

-- | Check if all parts of a composition type are in the category
checkInCategory :: Set String -> [String] -> Except String ()
checkInCategory _ [] = return ()
checkInCategory pieces (p:ps) = if Set.member p pieces
    then checkInCategory pieces ps
    else throwError $ "checkInCategory: Piece: " ++ show p ++ " not found in category"