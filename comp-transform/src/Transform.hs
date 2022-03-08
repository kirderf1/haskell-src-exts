{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform where

import Language.Haskell.Exts

import Types
import Decls
import Exps

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

-- | Set of all piece constructors
type Constrs = Set String

-- | Transform monad containing signature of categories and handles error messages as Strings.
type Transform = ReaderT (Sig, Constrs) (Except String)

-- | Transform a module by building signature of categories and then transforming the content of the module
transform :: Module () -> Except String (Module ())
transform m@(Module _ _mhead _pragmas _imports decls) = do
    sigCat <- buildSigCat decls
    sig    <- buildSigPiece decls sigCat
    constrs <- buildConstrs decls
    runReaderT (transformModule m) (sig, constrs) 
transform _xml = throwError "transform not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a module to remove syntax for composable types if the pragma is present
transformModule :: Module () -> Transform (Module ())
transformModule m@(Module _ mhead pragmas imports decls) =
    if pragmasContain "ComposableTypes" pragmas
        then do
            let pragmas' = modifyPragmas pragmas
                imports' = modifyImports imports
            mapDecl transformDecl 
                =<< mapExp transformExp
                =<< mapType transformType (Module () mhead pragmas' imports' decls)
        else return m
transformModule _xml = throwError "transformModule not defined for xml formats" 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a top level declaration to one or more new declarations
transformDecl :: Decl () -> Transform [Decl ()]
transformDecl (PieceDecl _ category headName cons derives) = 
    let parname = getParName
        cspar = map (parametConstructor parname category) cons
        in
    return (DataDecl 
        ()
        (DataType ())
        Nothing 
        (DHApp () (DHead () headName) (UnkindedVar () parname))
        cspar
        (deriveFunctor : derives)
        : [deriveTHPiece headName])
transformDecl (PieceCatDecl _ _) = return []
transformDecl (CompFunDecl _ names t) = concat <$> (declsForName `mapM` names)
  where
    declsForName :: Name () -> Transform [Decl ()]
    declsForName nam = do
        className <- toClassName nam
        funcName <- toFuncName nam
        classDecl <- functionClass className funcName t
        sigDecl <- functionsig nam className t
        return [classDecl, sigDecl, functionBind nam funcName, noinline nam, liftSum className]
transformDecl (CompFunInst _ funName pieceName Nothing) = do
    instHead <- createInstHead funName pieceName
    return [InstDecl () Nothing instHead Nothing]
transformDecl (CompFunInst _ funName pieceName (Just instDecls)) = do 
    instHead <- createInstHead funName pieceName
    instDecls' <- mapM transformInstDecl instDecls
    return [InstDecl () Nothing instHead (Just instDecls')]

transformDecl d = return [d]

-- | Transform a type
transformType :: Type () -> Transform (Type ())
transformType (TyComp _ category types) = do
    (cats, _) <- ask
    catStr <- lift $ qNameStr "TyComp" category 
    case Map.lookup category cats of
        Nothing -> throwError $ "Trying to form type of unknown category: " ++ catStr
        Just pieces -> do 
            typeNames <- mapM (lift . pieceName) types
            lift $ checkInCategory catStr pieces typeNames
            coprodtype <- coprod types
            return $ TyApp () termName (TyParen () coprodtype)
      where pieceName :: QName () -> Except String String
            pieceName qnam@(Qual _ _moduleName nam) = nameStr ("TyComp with " ++ show qnam) nam
            pieceName qnam@(UnQual _ nam) = nameStr ("TyComp with " ++ show qnam) nam
            pieceName _ = throwError "Unexpected special QName in part of TyComp."
transformType t = return t

-- | Transform an expression
transformExp :: Exp () -> Transform (Exp ())
transformExp a@(App _ (Con _ qcon) expr) = do
    (_, constrs) <- ask
    conStr <- lift $ qNameStr ("App with " ++ show qcon) qcon
    if Set.member conStr constrs
        then return $ App () (Var () (UnQual () (Ident () ("i" ++ conStr)))) expr
        else return a
transformExp a@(InfixApp _ expr1 (QConOp _ qcon) expr2) = do
    (_, constrs) <- ask
    conStr <- lift $ qNameStr ("InfixApp with " ++ show qcon) qcon
    if Set.member conStr constrs
        then return $ InfixApp () expr1
            (QVarOp () (UnQual () (Ident () ("i" ++ conStr)))) expr2
        else return a
transformExp e = return e

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
      (IHCon () (UnQual () (name "Functor")))]



-- | Get a name for the parametrized variable.            
-- TODO: Figure out how to handle unique names for parametrize variables
getParName :: Name ()
getParName = name "a"


-- | Template Haskell derive for a piece
deriveTHPiece :: Name () -> Decl () 
deriveTHPiece pieceName = deriveTH pieceName ["makeTraversable", "makeFoldable", "makeEqF",
                                  "makeShowF", "smartConstructors", "smartAConstructors"]                                

-- | Template Haskell derive for a certain data type from a list of things to derive
deriveTH :: Name () -> [String] -> Decl () 
deriveTH targetName list = SpliceDecl () 
        (SpliceExp ()
            (ParenSplice ()
                (app
                    (app
                        (qvar (ModuleName () "Data.Comp.Derive") (name "derive")) 
                        (List () (map deriveTHListElem list))
                    ) 
                    (List () [TypQuote () (UnQual () targetName)])
                )
            )
        )


-- | Element for a thing to derive with Template Haskell
deriveTHListElem :: String -> Exp ()
deriveTHListElem nam = qvar (ModuleName () "Data.Comp.Derive") (name nam)


-- TODO: possibly more pragmas?
-- | Modify a list of pragmas to remove ComposableTypes and add the ones needed for compdata
modifyPragmas :: [ModulePragma ()] -> [ModulePragma ()]
modifyPragmas ps =  foldr addPragma (removeCompTypes ps)
                                ["DeriveFunctor","TemplateHaskell","TypeOperators"
                                ,"FlexibleContexts"] 
    where  
        addPragma :: String -> [ModulePragma ()] -> [ModulePragma ()]
        addPragma nam prs = if pragmasContain nam prs 
                                 then prs
                                 else LanguagePragma () [name nam] : prs
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
                      (UnpromotedName () (Qual () (ModuleName () "Data.Comp") (sym ":+:")))
                       rest)
coprod _ = throwError "Trying to form coproduct of no arguments"

-- | Build signature of categories with empty maps
buildSigCat :: [Decl ()] -> Except String Sig
buildSigCat [] = return Map.empty
buildSigCat ((PieceCatDecl _ category):decls) = do
    sig <- buildSigCat decls
    let category' = UnQual () category

    case Map.lookup category' sig of
         Just _ -> throwError $ "buildSigCat: category " ++ show category' ++ " already declared"
         Nothing -> return $ Map.insert category' Set.empty sig
buildSigCat (_:decls) = buildSigCat decls
    
-- | Build signature, add pieces to map of categories
buildSigPiece :: [Decl ()] -> Sig -> Except String Sig
buildSigPiece [] sig = return sig
buildSigPiece  ((PieceDecl _ category headName _cons _derives):decls) sig = do
    sig' <- buildSigPiece decls sig
    headStr <- nameStr ("PieceDecl with " ++ show headName) headName
    case Map.lookup category sig' of
        Just oldCons -> return $ Map.insert category (Set.insert headStr oldCons) sig'
        Nothing -> do
            catStr <- qNameStr ("Category in PieceDecl") category 
            throwError $ "Category \"" ++ catStr ++ "\" not declared."
buildSigPiece (_:decls) sig = buildSigPiece decls sig

-- | Build set of all piece constructors
buildConstrs :: [Decl ()] -> Except String Constrs
buildConstrs [] = return Set.empty
buildConstrs ((PieceDecl _ _category _headName cons _derives):decls) = do
    constrs <- buildConstrs decls
    nameStrs <- mapM (conStr ("Constructors of PieceDecl with " ++ show cons)) cons
    return $ foldr Set.insert constrs nameStrs
    where conStr :: String -> QualConDecl () -> Except String String
          conStr err (QualConDecl _ _mForAll _mContext (ConDecl _ nam _types)) =
              nameStr err nam
          conStr err (QualConDecl _ _mForAll _mContext (InfixConDecl _ _type nam _types)) =
              nameStr err nam
          conStr err (QualConDecl _ _mForAll _mContext (RecDecl _ nam _fdecls)) = 
              nameStr err nam
buildConstrs (_:decls) = buildConstrs decls

-- | Modify a list of import declarations to add the ones needed for compdata
modifyImports :: [ImportDecl ()] -> [ImportDecl ()]
modifyImports is =  foldr addImport is
                                ["Data.Comp", "Data.Comp.Derive",
                                 "Data.Comp.Show ()", "Data.Comp.Equality ()"] 
    where  
        addImport :: String -> [ImportDecl ()] -> [ImportDecl ()]
        addImport nam is1 = if importsContain nam is1 
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
toFuncName nam = (\n -> name (n ++ "'")) <$> nameStr ("CompFuncDecl with " ++ show nam) nam

-- | Transform function name to class name, with capital first letter
toClassName :: Name () -> Transform (Name ())
toClassName nam = do
    str <- nameStr ("CompFunDecl with " ++ show nam) nam
    let (s:ss) = str
    return $ name (toUpper s : ss)
    

-- | Build a declaration of a class corresponding to a function
functionClass :: Name () -> Name () -> Type () -> Transform (Decl ())
functionClass className functionName t = do
    funType <- transformFunType className (TyApp () (TyVar () (name "f")) (TyParen () termType)) t
    return $ ClassDecl () Nothing
        (DHApp () (DHead () className) (UnkindedVar () (name "f"))) []
        (Just [classFunctionDecl functionName funType])

        
-- | Build the inner class declaration
classFunctionDecl :: Name () -> Type () -> ClassDecl ()
classFunctionDecl functionName t = ClsDecl () (TypeSig () [functionName] t)

-- | Build function type
transformFunType :: Name () -> Type () -> Type () -> Transform (Type ())
transformFunType cname replType ty = do
    (sig, _) <- ask
    resT <- mapType (convType sig) ty
    return (TyForall () Nothing (Just (CxSingle () (ParenA () (TypeA () (TyApp () (TyCon () (UnQual () cname)) (TyVar () (name "g"))))))) resT)
  where
    convType sig t = return (fromMaybe t (maybeConvType sig replType t))

-- | Maybe convert type if it matches a piece in signature
maybeConvType :: Sig -> Type () -> Type () -> Maybe (Type ())
maybeConvType sig replType (TyCon _ qname) = do
    if Map.member qname sig
      then Just replType
      else Nothing
maybeConvType _ _ _ = Nothing

-- | Build type for term with parametric part "g"
termType :: Type ()
termType = TyApp () termName (TyVar () (name "g"))

-- | Build type of name for term 
termName :: Type ()
termName = TyCon () (Qual () (ModuleName () "Data.Comp") (name "Term"))

-- | Build function signature
functionsig :: Name () -> Name () -> Type () -> Transform (Decl ())
functionsig nam className t = do
    funType <- transformFunType className termType t
    return $ TypeSig () [nam] funType

-- | Build declaration of final function that combines the class function with unTerm
functionBind :: Name () -> Name () -> Decl ()
functionBind nam funcName = patBind (pvar nam) (infixApp (var funcName) (op (sym ".")) (qvar (ModuleName () "Data.Comp") (name "unTerm")))

noinline :: Name () -> Decl ()
noinline nam = InlineSig () False Nothing (UnQual () nam)

-- | Derives liftSum for the function class
liftSum :: Name () -> Decl ()
liftSum className = SpliceDecl () (SpliceExp () (ParenSplice () (app (app (deriveTHListElem "derive") (List () [deriveTHListElem "liftSum"])) (List () [TypQuote () (UnQual () className)]))))

-- | Check if all parts of a composition type are in the category
checkInCategory :: String -> Set String -> [String] -> Except String ()
checkInCategory _ _ [] = return ()
checkInCategory category pieces (p:ps) = if Set.member p pieces
    then checkInCategory category pieces ps
    else throwError $ "Piece: " ++ show p ++ " not found in category: " ++ category
    
    
-- | Create instance head (roughly the first line of an instance declaration)
createInstHead :: QName () -> QName () -> Transform (InstRule ())
createInstHead funName pieceName = do
    className <- toClassQName funName
    return $ IRule () Nothing Nothing (IHApp () (IHCon () className) (TyCon () pieceName))
    where toClassQName (Qual _ moduleName fname) = do
                cname <- toClassName fname
                return $ Qual () moduleName cname
          toClassQName (UnQual _ fname) = do 
                cname <- toClassName fname
                return $ UnQual () cname
          toClassQName _ = throwError "Unexpected special qname of function name"


-- | Transform an instance declaration to have the function with a prime
transformInstDecl :: InstDecl () -> Transform (InstDecl ())
transformInstDecl (InsDecl _ (FunBind () matches)) = do 
    matches' <- mapM transformMatch matches
    return $ InsDecl () (FunBind () matches')
transformInstDecl _ = throwError "Unexpected type of instance declaration"
-- TODO: Possibly other constructs for InstDecl

-- | Transform function part of the instance declaration to have a prime on function name
transformMatch :: Match () -> Transform (Match ())
transformMatch (Match _ funName patterns rhs maybeBinds) = do
    funName' <- toFuncName funName
    return (Match () funName' patterns rhs maybeBinds)
transformMatch (InfixMatch _ pat funName patterns rhs maybeBinds) = do
    funName' <- toFuncName funName
    return (InfixMatch () pat funName' patterns rhs maybeBinds)
    
-- | Return string part of a name with custom error message
nameStr :: MonadError String m => String -> Name () -> m String
nameStr _ (Ident _ str) = return str
nameStr err _ = throwError $ "Expected ident name in " ++ err ++  ", but it was not that." 

-- | Return string part of QName with custom error message
qNameStr :: String -> QName () -> Except String String
qNameStr err (Qual _ _moduleName nam) = nameStr err nam
qNameStr err (UnQual _ nam) = nameStr err nam
qNameStr err _ = throwError $ "Unexpected special QName in " ++ err
