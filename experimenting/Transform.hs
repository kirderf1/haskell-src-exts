module Transform where

import Language.Haskell.Exts

-- import           Data.Map   (Map)
-- import qualified Data.Map as Map

-- perhaps what a state to keep track of the categories
data Env l = Env {
    categories :: [(Name l , [Name l])] -- Map Category [Name]
}

transform :: ParseResult (Module SrcSpanInfo) -> ParseResult (Module SrcSpanInfo)
transform f@ParseFailed{} = f
transform (ParseOk ast) = ParseOk $ transformModule ast


transformModule :: Module SrcSpanInfo -> Module SrcSpanInfo
transformModule (Module srcinfo mhead ps is ds) = do
    let ds' = map transformDecl ds
    (Module srcinfo mhead ps is ds')
-- Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
transformModule xml = xml 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

transformDecl :: Decl SrcSpanInfo -> Decl SrcSpanInfo
transformDecl (PieceDecl srcinfo cat nam cs ders) = 
    -- add cat to list of categories
    let parname = getParName (ann nam)
        cspar = map (parametConstructor parname cat) cs
        qdecls = map conDeclToQual cspar
        aders = head $ map ann ders
        functorDerive = deriveFunctor nam parname aders
        in
    (DataDecl 
        srcinfo 
        (DataType srcinfo)
        Nothing 
        (DHApp (ann nam) (DHead (ann nam) nam) (UnkindedVar (ann nam) parname))
        qdecls
        (functorDerive : ders)
        )


transformDecl d = d

-- TODO: make sure this changes the recursive part to a parametric variable
-- i.e. Pair Expr Expr should become Pair a a
parametConstructor :: Eq l => Name l -> Name l -> ConDecl l -> ConDecl l
parametConstructor parname cat (ConDecl l1 cname types) = 
    ConDecl l1 cname (map (parametType parname) types)
    where 
        parametType pname (TyVar tl recu) = 
            if recu == cat
                then TyVar tl pname
                else TyVar tl recu
        parametType _ t = t
parametConstructor _ _ c = c 

conDeclToQual :: ConDecl l -> QualConDecl l
conDeclToQual c = QualConDecl (ann c) Nothing Nothing c

deriveFunctor :: Name l -> Name l -> l -> Deriving l 
deriveFunctor nam parname aders =
  Deriving aders (Just (DerivStock aders))
    [IRule aders Nothing 
      (Just
        (CxSingle
          aders
          (TypeA -- ClassA (seems not to exist)
            aders --(UnQual aders (Ident aders "Functor")) [ TyVar aders name ])))
            (TyVar aders nam) )))
      (IHApp
        aders
        (IHCon aders (UnQual aders (Ident aders "Functor")))
        (TyParen
          aders
          (TyApp
            aders
            (TyCon aders (UnQual aders nam))
            (TyVar aders parname))))]



getParName :: l -> Name l 
getParName = undefined



deriveTH = undefined
-- Something like:
-- take list of all categories and for all of them: 
--      make a derive statement for makeFoldable, smartConstructors etc that are always needed.
--      perhaps look at if there is some things that should only be derived in some cases
-- also derive liftSum for the coproduct type