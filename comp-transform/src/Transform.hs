{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform (transform) where

import Language.Haskell.Exts

import Types
import Decls
import Exps

import           Data.Map   (Map)
import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set
import           Data.Char (toUpper)
import           Data.Maybe (catMaybes)

import Control.Monad.Reader
import Control.Monad.Except

-- | Map of category names to pieces
type Sig = Map (QName ()) (Set (QName ()))

-- | Set of all piece constructors
type Constrs = Set (QName ())

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
transformDecl (CompFunDecl _ names _mtvs mccx mcx category t) = do
    (sig, _) <- ask
    if Map.member category sig
      then concat <$> (declsForName `mapM` names)
      else do
          catStr <- lift $ qNameStr "CompFunDecl" category
          throwError $ "Expected first argument to be a piece category, was: \"" ++ catStr ++ "\""
  where
    declsForName :: Name () -> Transform [Decl ()]
    declsForName nam = do
        (mcx', vars) <- case mccx of 
                Nothing -> return (mcx, [])
                Just ccx -> transformCompContext ccx mcx 
        t' <- mapType (exchangeToTerm vars) t
        className <- toClassName nam
        funcName <- toFuncName nam
        classDecl <- functionClass mcx' className funcName t'
        sigDecl <- functionsig nam className t'
        return [classDecl, sigDecl, functionBind nam funcName, noinline nam, liftSum className]
transformDecl (CompFunExt _ mtvs mccx mcx funName types pieceName Nothing) = do
    instHead <- createInstHead mtvs mccx mcx funName types pieceName
    return [InstDecl () Nothing instHead Nothing]
transformDecl (CompFunExt _ mtvs mccx mcx funName types pieceName (Just instDecls)) = do 
    instHead <- createInstHead mtvs mccx mcx funName types pieceName
    instDecls' <- mapM transformInstDecl instDecls
    return [InstDecl () Nothing instHead (Just instDecls')]

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
      Nothing Nothing 
      (IHCon () (UnQual () (name "Functor")))]



-- | Get a name for the parametrized variable.            
-- TODO: Figure out how to handle unique names for parametrize variables
getParName :: Name ()
getParName = name "composable_types_recursive_var"


-- | Template Haskell derive for a piece
deriveTHPiece :: Name () -> Decl () 
deriveTHPiece pieceName = deriveTH pieceName ["smartConstructors"]

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
functionClass :: Maybe (Context ()) -> Name () -> Name () -> Type () -> Transform (Decl ())
functionClass mcx className functionName t = do
    funType <- transformFunType className (TyApp () (TyVar () (name "f")) (TyParen () termType)) t
    return $ ClassDecl () mcx
        (buildType $ collectUniqueVars t) []
        (Just [classFunctionDecl functionName funType])
  where
    buildType []         = DHApp () (DHead () className) (UnkindedVar () (name "f"))
    buildType (v:vars) = DHApp () (buildType vars) (UnkindedVar () v)

        
-- | Build the inner class declaration
classFunctionDecl :: Name () -> Type () -> ClassDecl ()
classFunctionDecl functionName t = ClsDecl () (TypeSig () [functionName] t)

-- | Build function type
transformFunType :: Name () -> Type () -> Type () -> Transform (Type ())
transformFunType cname replType ty = do
    let resT = TyFun () replType ty
    return (TyForall () Nothing Nothing (Just (CxSingle () (ParenA () (TypeA () (buildType $ collectUniqueVars ty))))) resT)
  where
    buildType []         = TyApp () (TyCon () (UnQual () cname)) (TyVar () (name "g"))
    buildType (v:vars) = TyApp () (buildType vars) (TyVar () v)

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
checkInCategory :: String -> Set (QName ()) -> [QName ()] -> Except String ()
checkInCategory _ _ [] = return ()
checkInCategory category pieces (p:ps) = if Set.member p pieces
    then checkInCategory category pieces ps
    else do
        pName <- qNameStr ("TyComp with " ++ show p) p
        throwError $ "Piece: " ++ pName ++ " not found in category: " ++ category
    
    
-- | Create instance head (roughly the first line of an instance declaration)
createInstHead :: Maybe [TyVarBind ()] -> Maybe (CompContext ()) -> Maybe (Context ()) -> Name () -> [Type ()] -> QName () -> Transform (InstRule ())
createInstHead mtvs mccx mcx funName types pieceName = do
    className <- toClassName funName
    case mccx of
         Nothing -> irule className mcx
         Just ccx -> do
             (mcx', _vs) <- transformCompContext ccx mcx 
             irule className mcx'
  where
    irule className mcx' = return $ IRule () mtvs Nothing mcx' (ihead className types)
    ihead className [] = IHApp () (IHCon () (UnQual () className)) (TyCon () pieceName)
    ihead className (t:ts) = IHApp () (ihead className ts) t

    
-- | Transform a qualified function name to a qualified class name
toClassQName :: QName () -> Transform (QName ())
toClassQName (Qual _ moduleName fname) = do
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
    
   
-- | Transform compcontext into regular context
transformCompContext :: CompContext () -> Maybe (Context ()) -> Transform ((Maybe (Context ()), [Name ()]))
transformCompContext (CompCxEmpty _) mcx = addToContext [] mcx
transformCompContext (CompCxSingle _ constraint) mcx = addToContext [constraint] mcx
transformCompContext (CompCxTuple _ constraints) mcx = addToContext constraints mcx

-- | Add constraints to context instead
addToContext :: [Constraint ()] -> Maybe (Context ()) -> Transform ((Maybe (Context ()), [Name ()]))
addToContext cs Nothing =  addToContext' cs []
addToContext cs (Just (CxEmpty _)) = addToContext' cs []
addToContext cs (Just (CxSingle _ asst)) =  addToContext' cs [asst]
addToContext cs (Just (CxTuple _ assts)) =  addToContext' cs assts
    
-- | Add constraints to assertions in a context
addToContext' :: [Constraint ()] -> [Asst ()] -> Transform ((Maybe (Context ()), [Name ()]))
addToContext' cs assts = do
    (assts', vars) <- addToAssts cs assts     
    return $ (Just (contextFromList assts'), vars)

-- | Create context from list of assertions
contextFromList :: [Asst ()] -> Context ()
contextFromList [] = CxEmpty ()
contextFromList [a] = CxSingle () a
contextFromList as = CxTuple () as

-- | Add constraints to list of assertions
addToAssts :: [Constraint ()] -> [Asst ()] -> Transform (([Asst ()], [Name ()]))
addToAssts cs as = do 
    asstvars <- mapM constraintToAsst cs
    let (csAssts, vars) = unzip asstvars
    return $ (catMaybes csAssts ++ as, vars)

-- | Transform constraint to assertion
constraintToAsst :: Constraint () -> Transform (Maybe (Asst ()), Name ())
constraintToAsst (FunConstraint _ fun v) = do
    cname <- toClassQName fun
    return (Just (TypeA () (TyApp () (TyCon () cname) (TyVar () v))), v) 
constraintToAsst (PieceConstraint _ piece v) = return (Just (TypeA () (TyInfix () (TyCon () piece) 
    (UnpromotedName () (Qual () (ModuleName () "Data.Comp") (Symbol () ":<:")))  (TyVar () v))), v)
constraintToAsst (CategoryConstraint _ _category v) = return (Nothing, v)

-- | Change a type variable to be a term
exchangeToTerm :: [Name ()] -> Type () -> Transform (Type ())
exchangeToTerm vars v@(TyVar _ vname) = if vname `elem` vars 
                                     then return $ TyApp () termName v
                                     else return v
exchangeToTerm _ t = return t
    
-- | Return string part of a name with custom error message
nameStr :: MonadError String m => String -> Name () -> m String
nameStr _ (Ident _ str) = return str
nameStr err _ = throwError $ "Expected ident name in " ++ err ++  ", but it was not that." 

-- | Return string part of QName with custom error message
qNameStr :: String -> QName () -> Except String String
qNameStr err (Qual _ _moduleName nam) = nameStr err nam
qNameStr err (UnQual _ nam) = nameStr err nam
qNameStr err _ = throwError $ "Unexpected special QName in " ++ err

collectUniqueVars :: Type () -> [Name ()]
collectUniqueVars = removeDups Set.empty . collectVars
  where
    removeDups _   []         = []
    removeDups set (v:vars) =
      if Set.member v set
        then removeDups set vars
        else v : removeDups (Set.insert v set) vars

class WithVar a where
    collectVars :: a l -> [Name l]

instance WithVar Type where
    collectVars t1 = case t1 of
          TyForall _ mtvs mccx mcx t    -> concatMap (concatMap collectVars) mtvs ++ concatMap collectVars mccx 
                                            ++ concatMap collectVars mcx ++ collectVars t
          TyStar  _                     -> []
          TyFun   _ t1' t2              -> collectVars t1' ++ collectVars t2
          TyTuple _ _ ts                -> concatMap collectVars ts
          TyUnboxedSum _ s              -> concatMap collectVars s
          TyList  _ t                   -> collectVars t
          TyParArray  _ t               -> collectVars t
          TyApp   _ t1' t2              -> collectVars t1' ++ collectVars t2
          TyVar   _ n                   -> [n]
          TyCon   _ qn                  -> []
          TyParen _ t                   -> collectVars t
          TyInfix _ ta _ tb             -> collectVars ta ++ collectVars tb
          TyKind  _ t k                 -> undefined
          TyPromoted _   p              -> undefined
          TyEquals _ a b                -> undefined
          TySplice _ s                  -> undefined
          TyBang _ b u t                -> undefined
          TyWildCard _ n                -> undefined
          TyQuasiQuote _ n s            -> undefined
          TyComp _ c t                  -> []


instance WithVar TyVarBind where
    collectVars (KindedVar   _ n _k) = [n]
    collectVars (UnkindedVar _ n)    = [n]

instance WithVar Context where
    collectVars (CxSingle _ asst ) = collectVars asst
    collectVars (CxTuple  _ assts) = concatMap collectVars assts
    collectVars (CxEmpty _) = []

instance WithVar Asst where
    collectVars asst = case asst of
        TypeA _ t           -> collectVars t
        IParam _ ipn t      -> undefined
        ParenA _ a          -> collectVars a

instance WithVar CompContext where
    collectVars (CompCxSingle _ c ) = collectVars c
    collectVars (CompCxTuple  _ cs) = concatMap collectVars cs
    collectVars (CompCxEmpty _) = []
    
instance WithVar Constraint where
    collectVars _ = undefined
