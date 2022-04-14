module FunctionTransform (transformFunDecl) where

import Language.Haskell.Exts

import TransformUtils
import Utils.Types
import Utils.Names

import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Except

-- | Transform a top level declaration to one or more new declarations
transformFunDecl :: Decl () -> Transform [Decl ()]
transformFunDecl (CompFunDecl _ names _mtvs mccx mcx category t) = do
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
transformFunDecl (CompFunExt _ mtvs mccx mcx funName types pieceName Nothing) = do
    instHead <- createInstHead mtvs mccx mcx funName types pieceName
    return [InstDecl () Nothing instHead Nothing]
transformFunDecl (CompFunExt _ mtvs mccx mcx funName types pieceName (Just instDecls)) = do 
    instHead <- createInstHead mtvs mccx mcx funName types pieceName
    instDecls' <- mapM transformInstDecl instDecls
    return [InstDecl () Nothing instHead (Just instDecls')]
transformFunDecl d = return [d]

-- | Transform function to function name with prime
toFuncName :: Name () -> Transform (Name ())
toFuncName nam = (\n -> name (n ++ "'")) <$> nameStr ("CompFuncDecl with " ++ show nam) nam  


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

