module Transform where

import Language.Haskell.Exts

-- import           Data.Map   (Map)
-- import qualified Data.Map as Map

-- perhaps have a state to keep track of the categories
data Env l = Env {
    categories :: [(Name l , [Name l])] -- Map Category [Name]
}

-- | Transform tree from parse result.
transform :: ParseResult (Module SrcSpanInfo) -> ParseResult (Module SrcSpanInfo)
transform f@ParseFailed{} = f
transform (ParseOk ast) = ParseOk $ transformModule ast


-- | Transform a module 
transformModule :: Module SrcSpanInfo -> Module SrcSpanInfo
transformModule m@(Module srcinfo mhead ps is ds) = do
    if pragmasContain "ComposableTypes" ps
        then do
            let ps' = modifyPragmas srcinfo ps
            let ds' = concatMap transformDecl ds
            (Module srcinfo mhead ps' is ds')
        else m
-- Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
transformModule xml = xml 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a top level declaration
transformDecl :: Decl SrcSpanInfo -> [Decl SrcSpanInfo]
transformDecl (PieceDecl srcinfo cat nam cs ders) = 
    -- add cat to list of categories
    let parname = getParName (ann nam)
        cspar = map (parametConstructor parname cat) cs
        aders = head $ map ann ders
        functorDerive = deriveFunctor nam parname aders
        in
    (DataDecl 
        srcinfo 
        (DataType srcinfo)
        Nothing 
        (DHApp (ann nam) (DHead (ann nam) nam) (UnkindedVar (ann nam) parname))
        cspar
        (functorDerive : ders)
        )
    : [deriveTHPiece nam]


transformDecl d = [d]

{- | Parametrize a piece constructor to have a parametrized variable as recursive 
    parameter instead of the name of the category it belongs to.
-}
parametConstructor :: Name l -> Name l -> QualConDecl l -> QualConDecl l
parametConstructor parname cat (QualConDecl l0 v c (ConDecl l1 cname types)) = 
    QualConDecl l0 v c (ConDecl l1 cname (map (parametType parname) types))
    where 
        -- TODO: Could there be other ways to form this construct?
        parametType pname (TyCon tl (UnQual t2 recu)) = 
            if recu `match` cat
                then TyCon tl (UnQual t2 pname)
                else TyCon tl (UnQual t2 recu)
        parametType _ t = t
        match (Ident _ n1) (Ident _ n2) = n1 == n2
        match _ _ = False
parametConstructor _ _ c = c 

-- | Wrap a ConDecl in a QualConDecl
-- Probably not needed anymore
conDeclToQual :: ConDecl l -> QualConDecl l
conDeclToQual c = QualConDecl (ann c) Nothing Nothing c


-- TODO: Add deriving functor in tuple of one derive, not in list. 
-- (If we want user to be able to add deriving clauses)
-- Gives this error message for multiple deriving clauses:
-- Illegal use of multiple, consecutive deriving clauses
-- Use DerivingStrategies to allow this

-- | Create a Deriving functor for a given data type
deriveFunctor :: Name l -> Name l -> l -> Deriving l 
deriveFunctor nam parname aders =
  Deriving aders Nothing --(Just (DerivStock aders))
    [IRule aders Nothing 
      Nothing
      (IHCon aders (UnQual aders (Ident aders "Functor")))]



-- | Get a name for the parametrized variable.            
-- TODO: Figure out how to handle unique names for parametrize variables
getParName :: l -> Name l 
getParName info = Ident info "a"


-- TODO: Check what should be derived by template Haskell, and build the corresponding tree.

deriveTHPiece :: Name l -> Decl l 
deriveTHPiece nam = deriveTH nam ["makeTraversable", "makeFoldable", "makeEqF",
                                  "makeShowF", "smartConstructors", "smartAConstructors"]                                


deriveTH :: Name l -> [String] -> Decl l 
deriveTH nam@(Ident l s) list = SpliceDecl l 
        (SpliceExp l 
            (ParenSplice l 
                (App l 
                    (App l 
                        (Var l
                            (UnQual l 
                                (Ident l "derive")
                            )
                        ) 
                        (List l 
                            (map (deriveTHListElem l) list)
                        )
                    ) 
                    (List l 
                        [TypQuote l
                            (UnQual l
                                nam
                            )
                        ]
                    )
                )
            )
        )

deriveTHListElem :: l -> String -> Exp l
deriveTHListElem l nam = Var l (UnQual l (Ident l nam))


-- Something like:
-- take list of all categories and for all of them: 
--      make a derive statement for makeFoldable, smartConstructors etc that are always needed.
--      perhaps look at if there is some things that should only be derived in some cases
-- also derive liftSum for the coproduct type


-- TODO: possibly more pragmas?
-- | Modify a list of pragmas to remove ComposableTypes and add DeriveFunctor, TemplateHaskell
modifyPragmas :: l -> [ModulePragma l] -> [ModulePragma l]
modifyPragmas l ps =  addPragma l "DeriveFunctor" $ addPragma l "TemplateHaskell" $ removeCompTypes ps
    where  
        addPragma :: l -> String -> [ModulePragma l] -> [ModulePragma l]
        addPragma l1 nam prs = if pragmasContain nam prs 
                                 then prs
                                 else (LanguagePragma l1 [Ident l1 nam]):prs
        removeCompTypes = filter (not . matchPragma "ComposableTypes")

pragmasContain :: String -> [ModulePragma l] -> Bool
pragmasContain nam = any (matchPragma nam)
        
matchPragma :: String -> ModulePragma l -> Bool
matchPragma s (LanguagePragma _ [Ident _ nam]) = nam == s
matchPragma _ _ = False

