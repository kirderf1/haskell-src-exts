{-# LANGUAGE FlexibleContexts #-}

module TransformUtils where

import Language.Haskell.Exts

import qualified GeneratedNames as Names

import           Data.Map   (Map)
import           Data.Set   (Set)
import           Data.Maybe (catMaybes)

import Control.Monad.Reader
import Control.Monad.Except

-- | Map of category names to pieces
type Sig = Map (QName ()) (Set (QName ()))

-- | Set of all piece constructors
type Constrs = Set (QName ())

-- | Transform monad containing signature of categories and handles error messages as Strings.
type Transform = ReaderT (Sig, Constrs) (Except String)

compdata :: ModuleName ()
compdata = ModuleName () "Data.Comp"

termApp :: Type () -> Type ()
termApp = TyApp () (TyCon () (Qual () compdata (name "Term")))

-- subName :: QName ()
-- subName = Qual () compdata (Symbol () ":<:")

lib :: ModuleName ()
lib = ModuleName () "ComposableTypes"

partOfName :: QName ()
partOfName = Qual () lib (Ident () "PartOf")

injectExp :: Exp ()
injectExp = qvar lib (name "inject'")

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
-- exchangeToTerm :: [Name ()] -> Type () -> Transform (Type ())
-- exchangeToTerm vars v@(TyVar _ vname) | vname `elem` vars = return $ termApp v
-- exchangeToTerm _ t = return t


-- | Transform compcontext into regular context
transformCompContext :: CompContext () -> Maybe (Context ()) -> Transform (Maybe (Context ()), [Name ()])
transformCompContext (CompCxEmpty _) mcx = addToContext [] mcx
transformCompContext (CompCxSingle _ constraint) mcx = addToContext [constraint] mcx
transformCompContext (CompCxTuple _ constraints) mcx = addToContext constraints mcx

-- | Add constraints to context instead
addToContext :: [Constraint ()] -> Maybe (Context ()) -> Transform (Maybe (Context ()), [Name ()])
addToContext cs Nothing =  addToContext' cs []
addToContext cs (Just (CxEmpty _)) = addToContext' cs []
addToContext cs (Just (CxSingle _ asst)) =  addToContext' cs [asst]
addToContext cs (Just (CxTuple _ assts)) =  addToContext' cs assts

-- | Add constraints to assertions in a context
addToContext' :: [Constraint ()] -> [Asst ()] -> Transform (Maybe (Context ()), [Name ()])
addToContext' cs assts = do
    (assts', vars) <- addToAssts cs assts   
    return $ (Just (contextFromList assts'), vars)

-- | Create context from list of assertions
contextFromList :: [Asst ()] -> Context ()
contextFromList [] = CxEmpty ()
contextFromList [a] = CxSingle () a
contextFromList as = CxTuple () as

-- | Add constraints to list of assertions
addToAssts :: [Constraint ()] -> [Asst ()] -> Transform ([Asst ()], [Name ()])
addToAssts cs as = do 
    asstvars <- mapM constraintToAsst cs
    let (csAssts, vars) = unzip asstvars
    return $ (catMaybes csAssts ++ as, vars)

-- | Transform constraint to assertion
constraintToAsst :: Constraint () -> Transform (Maybe (Asst ()), Name ())
constraintToAsst (FunConstraint _ fun v) = do
    cname <- Names.qOuterClass fun
    return (Just (TypeA () (TyApp () (TyCon () cname) (TyVar () v))), v) 
constraintToAsst (PieceConstraint _ piece v) = return (Just (TypeA () (TyApp () 
    (TyApp () (TyCon () partOfName)  (TyCon () piece)) (TyVar () v))), v)
constraintToAsst (CategoryConstraint _ _category v) = return (Nothing, v)


transformContext :: Context () -> Transform (Context ())
transformContext (CxEmpty _) = return (CxEmpty ())
transformContext (CxSingle _ asst) = transformContext' [asst]     
transformContext (CxTuple _ assts) = transformContext' assts

transformContext' :: [Asst ()] -> Transform (Context ())
transformContext' assts = do
    asstvars <- mapM transformAsst assts 
    let (assts', _vars) = unzip asstvars
    return (contextFromList (catMaybes assts'))

transformAsst :: Asst () -> Transform (Maybe (Asst ()), Maybe (Name ()))
transformAsst (CompCont _ constraint) = do
    (masst, v) <- constraintToAsst constraint
    return (masst, (Just v))
transformAsst (ParenA _ asst) = transformAsst asst
transformAsst asst = return (Just asst, Nothing) 
