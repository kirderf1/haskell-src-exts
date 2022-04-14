{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform (transform) where

import Language.Haskell.Exts

import FunctionTransform
import TransformUtils
import Utils.Types
import Utils.Decls
import Utils.Exps

import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.Except

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
            mapDecl transformFunDecl 
                =<< mapDecl transformDecl
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
transformDecl d = return [d]

-- | Transform a type
transformType :: Type () -> Transform (Type ())
transformType (TyForall _ mfa (Just ccx) mcx t) = do
    (mcx', vars) <- transformCompContext ccx mcx 
    t' <- mapType (exchangeToTerm vars) t
    return $ TyForall () mfa Nothing mcx' t'
transformType (TyComp _ category types) = do
    (cats, _) <- ask
    catStr <- lift $ qNameStr "TyComp" category 
    case Map.lookup category cats of
        Nothing -> throwError $ "Trying to form type of unknown category: " ++ catStr
        Just pieces -> do 
            lift $ checkInCategory catStr pieces types
            coprodtype <- coprod types
            return $ TyApp () termName (TyParen () coprodtype)
transformType t = return t

-- | Transform an expression
transformExp :: Exp () -> Transform (Exp ())
transformExp a@(Con _ qcon) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- toSmartCon qcon
             return $ Var () smartCon
        else return a
transformExp a@(InfixApp _ expr1 (QConOp _ qcon) expr2) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- toSmartCon qcon
             return $ InfixApp () expr1 (QVarOp () smartCon) expr2
        else return a
transformExp a@(LeftSection _ expr (QConOp _ qcon)) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- toSmartCon qcon
             return $ LeftSection () expr (QVarOp () smartCon)
        else return a
transformExp a@(RightSection _ (QConOp _ qcon) expr) = do
    (_, constrs) <- ask
    if Set.member qcon constrs
        then do
             smartCon <- toSmartCon qcon
             return $ RightSection () (QVarOp () smartCon) expr
        else return a
transformExp e = return e

toSmartCon :: QName () -> Transform (QName ())
toSmartCon (UnQual ()            (Ident () str)) = return $ UnQual ()            (Ident () ('i' : str))
toSmartCon (Qual   () moduleName (Ident () str)) = return $ Qual   () moduleName (Ident () ('i' : str))
toSmartCon qname                                 = throwError $ "Tried to transform unexpected expression \"" ++ prettyPrint qname ++ "\"." 

{- | Parametrize a piece constructor to have a parametrized variable as recursive 
    parameter instead of the name of the category it belongs to.
-}
parametConstructor :: Name () -> QName () -> QualConDecl () -> QualConDecl ()
parametConstructor parname category (QualConDecl _ v c conDecl) = 
    QualConDecl () v c (parametCon conDecl)
    where 
        parametCon (ConDecl      _ cname types)       = ConDecl      () cname (parametType <$> types)
        parametCon (InfixConDecl _ type1 cname type2) = InfixConDecl () (parametType type1) cname (parametType type2)
        parametCon (RecDecl      _ cname fields)      = RecDecl      () cname (parametField <$> fields)
        
        parametField (FieldDecl _ names ty) = FieldDecl () names (parametType ty)
        
        parametType (TyCon _ recu) | recu == category = TyVar () parname
        parametType t = t


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
      Nothing Nothing 
      (IHCon () (UnQual () (name "Functor")))]



-- | Get a name for the parametrized variable.            
-- TODO: Figure out how to handle unique names for parametrize variables
getParName :: Name ()
getParName = name "composable_types_recursive_var"

-- TODO: possibly more pragmas?
-- | Modify a list of pragmas to remove ComposableTypes and add the ones needed for compdata
modifyPragmas :: [ModulePragma ()] -> [ModulePragma ()]
modifyPragmas ps =  foldr addPragma (removeCompTypes ps)
                                ["DeriveFunctor","TemplateHaskell","TypeOperators"
                                ,"FlexibleContexts","FlexibleInstances","MultiParamTypeClasses"
                                ,"UndecidableInstances"] 
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
    case Map.lookup category sig' of
        Just oldCons -> return $ Map.insert category (Set.insert (UnQual () headName) oldCons) sig'
        Nothing -> do
            catStr <- qNameStr ("Category in PieceDecl") category 
            throwError $ "Category \"" ++ catStr ++ "\" not declared."
buildSigPiece (_:decls) sig = buildSigPiece decls sig

-- | Build set of all piece constructors
buildConstrs :: [Decl ()] -> Except String Constrs
buildConstrs [] = return Set.empty
buildConstrs ((PieceDecl _ _category _headName cons _derives):decls) = do
    constrs <- buildConstrs decls
    return $ foldr Set.insert constrs (qualConName <$> cons)
    where qualConName (QualConDecl _ _mForAll _mContext conDecl) = UnQual () (conName conDecl)
          conName (ConDecl _ nam _types) = nam
          conName (InfixConDecl _ _type nam _types) = nam
          conName (RecDecl _ nam _fdecls) = nam
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

-- | Check if all parts of a composition type are in the category
checkInCategory :: String -> Set (QName ()) -> [QName ()] -> Except String ()
checkInCategory _ _ [] = return ()
checkInCategory category pieces (p:ps) = if Set.member p pieces
    then checkInCategory category pieces ps
    else do
        pName <- qNameStr ("TyComp with " ++ show p) p
        throwError $ "Piece: " ++ pName ++ " not found in category: " ++ category

