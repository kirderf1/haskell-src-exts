{-# LANGUAGE FlexibleContexts #-}

module TransformUtils where

import Language.Haskell.Exts

import           Data.Map   (Map)
import           Data.Set   (Set)
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

-- | Return string part of a name with custom error message
nameStr :: MonadError String m => String -> Name () -> m String
nameStr _ (Ident _ str) = return str
nameStr err _ = throwError $ "Expected ident name in " ++ err ++  ", but it was not that." 

-- | Return string part of QName with custom error message
qNameStr :: String -> QName () -> Except String String
qNameStr err (Qual _ _moduleName nam) = nameStr err nam
qNameStr err (UnQual _ nam) = nameStr err nam
qNameStr err _ = throwError $ "Unexpected special QName in " ++ err

-- | Transform function name to class name, with capital first letter
toClassName :: Name () -> Transform (Name ())
toClassName nam = do
    str <- nameStr ("CompFunDecl with " ++ show nam) nam
    let (s:ss) = str
    return $ name (toUpper s : ss)
  
-- | Transform a qualified function name to a qualified class name
toClassQName :: QName () -> Transform (QName ())
toClassQName (Qual _ moduleName fname) = do
                cname <- toClassName fname
                return $ Qual () moduleName cname
toClassQName (UnQual _ fname) = do 
                cname <- toClassName fname
                return $ UnQual () cname
toClassQName _ = throwError "Unexpected special qname of function name"

-- | Build type of name for term 
termName :: Type ()
termName = TyCon () (Qual () (ModuleName () "Data.Comp") (name "Term"))

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

-- | Change a type variable to be a term
exchangeToTerm :: [Name ()] -> Type () -> Transform (Type ())
exchangeToTerm vars v@(TyVar _ vname) | vname `elem` vars = return $ TyApp () termName v
exchangeToTerm _ t = return t


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

