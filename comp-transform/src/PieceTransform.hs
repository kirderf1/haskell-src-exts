
module PieceTransform (transformPieceDecl, transformCompType, toSmartCon) where

import Language.Haskell.Exts

import TransformUtils

import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set

import Control.Monad.Reader
import Control.Monad.Except

-- | Transform a top level declaration to one or more new declarations
transformPieceDecl :: Decl () -> Transform [Decl ()]
transformPieceDecl (PieceDecl _ category pieceName cons derives) = 
    let parname = getParName
        cspar = map (parametConstructor parname category) cons
        in do
    smartCons <- concat <$> mapM (smartCon category pieceName) cons
    return (DataDecl 
        ()
        (DataType ())
        Nothing 
        (DHApp () (DHead () pieceName) (UnkindedVar () parname))
        cspar
        (deriveFunctor : derives)
        : smartCons)
transformPieceDecl (PieceCatDecl _ _) = return []
transformPieceDecl d = return [d]

-- | Transform a type
transformCompType :: Type () -> Transform (Type ())
transformCompType (TyComp _ category types) = do
    (cats, _) <- ask
    catStr <- lift $ qNameStr "TyComp" category 
    case Map.lookup category cats of
        Nothing -> throwError $ "Trying to form type of unknown category: " ++ catStr
        Just pieces -> do 
            lift $ checkInCategory catStr pieces types
            coprodtype <- coprod types
            return $ termApp (TyParen () coprodtype)
transformCompType t = return t

-- | Get a name for the parametrized variable.
getParName :: Name ()
getParName = name "composable_types_recursive_var"


-- | Form coproduct type from a list of pieces
coprod :: [QName ()] -> Transform (Type ())
coprod [nam] = return $ TyCon () nam
coprod (nam:ns) = do
    rest <- coprod ns
    return (TyInfix () (TyCon () nam)
                      (UnpromotedName () (Qual () (ModuleName () "Data.Comp") (sym ":+:")))
                       rest)
coprod _ = throwError "Trying to form coproduct of no arguments"

-- | Check if all parts of a composition type are in the category
checkInCategory :: String -> Set (QName ()) -> [QName ()] -> Except String ()
checkInCategory _ _ [] = return ()
checkInCategory category pieces (p:ps) = if Set.member p pieces
    then checkInCategory category pieces ps
    else do
        pName <- qNameStr ("TyComp with " ++ show p) p
        throwError $ "Piece: " ++ pName ++ " not found in category: " ++ category

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

collectTypes :: QualConDecl () -> (Name (), [Type ()])
collectTypes (QualConDecl _ _ _ conDecl) = conDeclArgs conDecl
  where
    conDeclArgs :: ConDecl () -> (Name (), [Type ()])
    conDeclArgs (ConDecl _ nam types) = (nam, types)
    conDeclArgs (InfixConDecl _ t1 nam t2) = (nam, [t1, t2])
    conDeclArgs (RecDecl _ nam fields) = (nam, concat $ map fieldTypes fields)
      where fieldTypes (FieldDecl _ names ty) = replicate (length names) ty

smartCon :: QName () -> Name () -> QualConDecl () -> Transform [Decl ()]
smartCon cat piece con = do
    funName <- toSmartName conName
    let pattern = pApp funName (pvar <$> argNames)
    return $ [typeBinding funName, patBind pattern expr]
  where
    (conName, argTypes) = collectTypes con
    args = length argTypes
    argNames = (\arg -> name $ "arg_" ++ show (arg)) <$> [1..args]
    internalType = TyVar () (name "g")
    term = termApp internalType
    
    replaceCat (TyCon () tycon) | tycon == cat = term
    replaceCat t                               = t
    funType = foldr (TyFun ()) term (replaceCat <$> argTypes)
    subAssertion = TypeA () (TyInfix () (TyCon () (UnQual () piece)) (UnpromotedName () subName) internalType)
    typeBinding funName = TypeSig () [funName] (TyForall () Nothing Nothing (Just (CxSingle () subAssertion)) funType)
    
    expr = app injectExp (foldl app (Con () $ UnQual () conName) (var <$> argNames))

toSmartCon :: QName () -> Transform (QName ())
toSmartCon (UnQual ()            nam) = UnQual ()            <$> toSmartName nam
toSmartCon (Qual   () moduleName nam) = Qual   () moduleName <$> toSmartName nam
toSmartCon qname                                 = throwError $ "Tried to transform unexpected constructor name \"" ++ prettyPrint qname ++ "\"." 

toSmartName :: Name () -> Transform (Name ())
toSmartName (Ident () str) = return $ name ("composable_types_constructor_" ++ str)
toSmartName nam           = throwError $ "Tried to transform unexpected constructor name \"" ++ prettyPrint nam ++ "\"."
