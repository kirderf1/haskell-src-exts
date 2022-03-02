{-# LANGUAGE ScopedTypeVariables #-}
module Transform where

import Language.Haskell.Exts

import Types

import           Data.Map   (Map)
import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set
import           Data.Maybe (fromMaybe)
import           Data.Char (toUpper)

import Control.Monad.Reader
import Control.Monad.Except

-- | Map of category names to pieces
type Sig = Map (QName ()) (Set String)

-- | Transform monad containing signature of categories and handles error messages as Strings.
type Transform = ReaderT Sig (Except String)

transform :: Module () -> Except String (Module ())
transform m@(Module _ _mhead _pragmas _imports decls) = do
    sigCat <- buildSigCat decls
    sig    <- buildSigPiece decls sigCat
    runReaderT (transformModule m) sig 
transform _xml = throwError "transform not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a module to remove syntax for composable types if the pragma is present
transformModule :: Module () -> Transform (Module ())
transformModule m@(Module _ mhead pragmas imports decls) =
    if pragmasContain "ComposableTypes" pragmas
        then do
            let pragmas' = modifyPragmas pragmas
                imports' = modifyImports imports
            
            decls' <- liftM concat (mapM transformDecl decls)
            mapType transformType (Module () mhead pragmas' imports' decls')
        else return m
transformModule _xml = throwError "transformModule not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a top level declaration to one or more new declarations
transformDecl :: Decl () -> Transform [Decl ()]
transformDecl (PieceDecl _ (category :: QName ()) headName cons derives) = 
    let parname = getParName
        cspar = map (parametConstructor parname category) cons
        in
    return ((DataDecl 
        ()
        (DataType ())
        Nothing 
        (DHApp () (DHead () headName) (UnkindedVar () parname))
        cspar
        (deriveFunctor : derives)
        )
        : [deriveTHPiece headName])
transformDecl (PieceCatDecl _ _) = return []
transformDecl (CompFunDecl () names t) = concat <$> (declsForName t `mapM` names)
  where
    declsForName :: Type () -> Name () -> Transform [Decl ()]
    declsForName t nam = do
        className <- toClassName nam
        funcName <- toFuncName nam
        classDecl <- functionClass className funcName t
        sigDecl <- functionsig nam className t
        return [classDecl, sigDecl, functionBind nam funcName, liftSum className]

transformDecl d = return [d]

-- | Transform a type
transformType :: Type () -> Transform (Type ())
transformType (TyComp _ category types) = do
    -- check if piece constructors are in category
    cats <- ask
    let cat = fmap (const ()) category
    case Map.lookup cat cats of
        Nothing -> throwError "transformType: Trying to form type of unknown category"
        Just pieces -> do 
            typeNames <- mapM (lift . catName) types
            lift $ checkInCategory pieces typeNames
            coprodtype <- coprod types
            return $ TyApp () termName (TyParen () coprodtype)
      where catName :: QName () -> Except String String
            catName (Qual _ _moduleName (Ident _ c)) = return c
            catName (UnQual _ (Ident _ c)) = return c
            catName _ = throwError "transformType: unexpected type of category name"
            -- TODO: special QName?
transformType t = return t

{- | Parametrize a piece constructor to have a parametrized variable as recursive 
    parameter instead of the name of the category it belongs to.
-}
parametConstructor :: Name () -> QName () -> QualConDecl () -> QualConDecl ()
parametConstructor parname category (QualConDecl _ v c (ConDecl _ cname types)) = 
    QualConDecl () v c (ConDecl () cname (map (parametType parname) types))
    where 
        -- TODO: Could there be other ways to form this construct?
        parametType pname (TyCon _ recu) = 
            if recu == category
                then TyCon () (UnQual () pname)
                else TyCon () recu
        parametType _ t = t
parametConstructor _ _ c = c 


-- TODO: Add deriving functor in tuple of one derive, not in list. 
-- (If we want user to be able to add deriving clauses)
-- Gives this error message for multiple deriving clauses:
-- Illegal use of multiple, consecutive deriving clauses
-- Use DerivingStrategies to allow this

-- | Create a Deriving functor for a given data type
deriveFunctor :: Deriving ()
deriveFunctor =
  Deriving () Nothing
    [IRule () Nothing 
      Nothing
      (IHCon () (UnQual () (Ident () "Functor")))]



-- | Get a name for the parametrized variable.            
-- TODO: Figure out how to handle unique names for parametrize variables
getParName :: Name ()
getParName = Ident () "a"


-- | Template Haskell derive for a piece
deriveTHPiece :: Name () -> Decl () 
deriveTHPiece pieceName = deriveTH pieceName ["makeTraversable", "makeFoldable", "makeEqF",
                                  "makeShowF", "smartConstructors", "smartAConstructors"]                                

-- | Template Haskell derive for a certain data type from a list of things to derive
deriveTH :: Name () -> [String] -> Decl () 
deriveTH targetName list = SpliceDecl () 
        (SpliceExp ()
            (ParenSplice ()
                (App ()
                    (App ()
                        (Var ()
                            (Qual () (ModuleName () "Data.Comp.Derive") (Ident () "derive"))
                        ) 
                        (List ()
                            (map deriveTHListElem list)
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
deriveTHListElem :: String -> Exp ()
deriveTHListElem nam = Var () (Qual () (ModuleName () "Data.Comp.Derive") (Ident () nam))


-- TODO: possibly more pragmas?
-- | Modify a list of pragmas to remove ComposableTypes and add the ones needed for compdata
modifyPragmas :: [ModulePragma ()] -> [ModulePragma ()]
modifyPragmas ps =  concatMap (addPragma (removeCompTypes ps)) 
                                ["DeriveFunctor","TemplateHaskell","TypeOperators"
                                ,"FlexibleContexts"] 
    where  
        addPragma :: [ModulePragma ()] -> String -> [ModulePragma ()]
        addPragma prs nam = if pragmasContain nam prs 
                                 then prs
                                 else (LanguagePragma () [Ident () nam]):prs
        removeCompTypes = filter (not . matchPragma "ComposableTypes")

-- | Check if the list of pragmas contain a certain one
pragmasContain :: String -> [ModulePragma ()] -> Bool
pragmasContain nam = any (matchPragma nam)
        
-- | Check if a pragma match the given String
matchPragma :: String -> ModulePragma () -> Bool
matchPragma s (LanguagePragma _ [Ident _ nam]) = nam == s
matchPragma _ _ = False

-- | Form coproduct type from a list of pieces
coprod :: [QName ()] -> Transform (Type ())
coprod [nam] = return $ TyCon () nam
coprod (nam:ns) = do
    rest <- coprod ns
    return (TyInfix () (TyCon () nam)
                      (UnpromotedName () (Qual () (ModuleName () "Data.Comp") (Symbol () ":+:")))
                       rest)
coprod _ = throwError "Trying to form coproduct of no arguments"

-- | Build signature of categories with empty maps
buildSigCat :: [Decl ()] -> Except String Sig
buildSigCat [] = return Map.empty
buildSigCat ((PieceCatDecl _l category):decls) = do
    sig <- buildSigCat decls
    let category' = (UnQual () (fmap (const ()) category))

    case Map.lookup category' sig of
         Just _ -> throwError $ "buildSigCat: category " ++ show category' ++ " already declared"
         Nothing -> return $ Map.insert category' Set.empty sig
buildSigCat (_:decls) = buildSigCat decls
    
-- | Build signature, add pieces to map of categories
buildSigPiece :: [Decl ()] -> Sig -> Except String Sig
buildSigPiece [] sig = return sig
buildSigPiece  ((PieceDecl _ (category :: QName ()) headName _cons _derives):decls) sig = do
    sig' <- buildSigPiece decls sig
    let category' = fmap (const ()) category
    idHead <- nameID headName
    case Map.lookup category' sig' of
        Just oldCons -> return $ Map.insert category' (Set.insert idHead oldCons) sig'
        Nothing -> throwError $ "buildSigPiece: category " ++ show category' ++ " not declared."
    where
        nameID :: Name () -> Except String String
        nameID (Ident _ s) = return s
        nameID _ = throwError "buildSigPiece: unexpected type of name"
buildSigPiece (_:decls) sig = buildSigPiece decls sig

-- | Modify a list of import declarations to add the ones needed for compdata
modifyImports :: [ImportDecl ()] -> [ImportDecl ()]
modifyImports is =  concatMap (addImport is)
                                ["Data.Comp", "Data.Comp.Derive",
                                 "Data.Comp.Show ()", "Data.Comp.Equality ()"] 
    where  
        addImport :: [ImportDecl ()] -> String -> [ImportDecl ()]
        addImport is1 nam = if importsContain nam is1 
                                 then is1
                                 else (ImportDecl
                                 { importAnn = ()                     -- ^ annotation, used by parser for position of the @import@ keyword.
                                 , importModule = ModuleName () nam   -- ^ name of the module imported.
                                 , importQualified = True            -- ^ imported @qualified@?
                                 , importSrc = False                  -- ^ imported with @{-\# SOURCE \#-}@?
                                 , importSafe = False                 -- ^ Import @safe@?
                                 , importPkg = Nothing                -- ^ imported with explicit package name
                                 , importAs = Nothing                 -- ^ optional alias name in an @as@ clause.
                                 , importSpecs = Nothing              -- ^ optional list of import specifications.
                                 }):is1


-- | Check if the list of import declarations contain a certain one
importsContain :: String -> [ImportDecl ()] -> Bool
importsContain nam = any (matchImport nam)
        
-- | Check if an import declaration match the given String
matchImport :: String -> ImportDecl () -> Bool
matchImport s (ImportDecl {importModule = ModuleName _ nam}) = nam == s

-- | Transform function to function name with prime
toFuncName :: Name () -> Transform (Name ())
toFuncName (Ident _ nam) = return $ Ident () (nam ++ "'")
toFuncName _             = throwError "Expected ident name in CompFunDecl, but it was not that."

-- | Transform function name to class name, with capital first letter
toClassName :: Name () -> Transform (Name ())
toClassName (Ident () (c:nam)) = return $ Ident () (toUpper c : nam)
toClassName _             = throwError "Expected ident name in CompFunDecl, but it was not that."

-- | Build a declaration of a class corresponding to a function
functionClass :: Name () -> Name () -> Type () -> Transform (Decl ())
functionClass className functionName t = do
    funType <- transformFunType className (TyApp () (TyVar () (Ident () "f")) (TyParen () termType)) t
    return $ ClassDecl () Nothing
        (DHApp () (DHead () className) (UnkindedVar () (Ident () "f"))) []
        (Just [classFunctionDecl functionName funType])

        
-- | Build the inner class declaration
classFunctionDecl :: Name () -> Type () -> ClassDecl ()
classFunctionDecl functionName t = ClsDecl () (TypeSig () [functionName] t)

-- | Build function type
transformFunType :: Name () -> Type () -> Type () -> Transform (Type ())
transformFunType cname replType t = do
    sig <- ask
    resT <- mapType (convType sig) t
    return (TyForall () Nothing (Just (CxSingle () (ParenA () (TypeA () (TyApp () (TyCon () (UnQual () cname)) (TyVar () (Ident () "g"))))))) resT)
  where
    convType sig t = return (fromMaybe t (maybeConvType sig replType t))

-- | Maybe convert type if it matches a piece in signature
maybeConvType :: Sig -> Type () -> Type () -> Maybe (Type ())
maybeConvType sig replType (TyCon () qname) = do
    if Map.member (const () <$> qname) sig
      then Just replType
      else Nothing
maybeConvType _ _ _ = Nothing

-- | Build type for term with parametric part "g"
termType :: Type ()
termType = TyApp () termName (TyVar () (Ident () "g"))

-- | Build type of name for term 
termName :: Type ()
termName = TyCon () (Qual () (ModuleName () "Data.Comp") (Ident () "Term"))

-- | Build function signature
functionsig :: Name () -> Name () -> Type () -> Transform (Decl ())
functionsig nam className t = do
    funType <- transformFunType className termType t
    return $ TypeSig () [nam] funType

-- | Build declaration of final function that combines the class function with unTerm
functionBind :: Name () -> Name () -> Decl ()
functionBind nam funcName = PatBind () (PVar () nam) (UnGuardedRhs () (InfixApp () (Var () (UnQual () funcName)) (QVarOp () (UnQual () (Symbol () "."))) (Var () (Qual () (ModuleName () "Data.Comp") (Ident () "unTerm"))))) Nothing

-- | Derives liftSum for the function class
liftSum :: Name () -> Decl ()
liftSum className = SpliceDecl () (SpliceExp () (ParenSplice () (App () (App () (deriveTHListElem "derive") (List () [deriveTHListElem "liftSum"])) (List () [TypQuote () (UnQual () className)]))))

-- | Check if all parts of a composition type are in the category
checkInCategory :: Set String -> [String] -> Except String ()
checkInCategory _ [] = return ()
checkInCategory pieces (p:ps) = if Set.member p pieces
    then checkInCategory pieces ps
    else throwError $ "checkInCategory: Piece: " ++ show p ++ " not found in category"
