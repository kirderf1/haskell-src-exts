module Transform where

import Language.Haskell.Exts

-- import           Data.Map   (Map)
-- import qualified Data.Map as Map

-- perhaps have a state to keep track of the categories
data Env l = Env {
    categories :: [(Name l , [Name l])] -- Map Category [Name]
}

-- | Transform a module to remove syntax for composable types if the pragma is present
transformModule :: Module l -> Module l
transformModule m@(Module l mhead pragmas imports decls) =
    if pragmasContain "ComposableTypes" pragmas
        then
            let pragmas' = modifyPragmas l pragmas
                decls' = concatMap transformDecl decls
            in (Module l mhead pragmas' imports decls')
        else m
transformModule xml = xml 
-- ^ XmlPage and XmlHybrid formats not handled (yet)

-- | Transform a top level declaration to one or more new declarations
transformDecl :: Decl l -> [Decl l]
transformDecl (PieceDecl l category headName cons derives) = 
    -- add cat to list of categories
    let nameL = ann headName
        parname = getParName nameL
        cspar = map (parametConstructor parname category) cons
        aders = head $ map ann derives
        functorDerive = deriveFunctor aders
        in
    (DataDecl 
        l 
        (DataType l)
        Nothing 
        (DHApp nameL (DHead nameL headName) (UnkindedVar nameL parname))
        cspar
        (functorDerive : derives)
        )
    : [deriveTHPiece headName]

transformDecl (TypeDecl l dhead (TyComp _l2 _nam types)) = 
    [TypeDecl l dhead (coprod types)]

transformDecl d = [d]

{- | Parametrize a piece constructor to have a parametrized variable as recursive 
    parameter instead of the name of the category it belongs to.
-}
parametConstructor :: Name l -> Name l -> QualConDecl l -> QualConDecl l
parametConstructor parname category (QualConDecl l0 v c (ConDecl l1 cname types)) = 
    QualConDecl l0 v c (ConDecl l1 cname (map (parametType parname) types))
    where 
        -- TODO: Could there be other ways to form this construct?
        parametType pname (TyCon tl (UnQual t2 recu)) = 
            if recu `match` category
                then TyCon tl (UnQual t2 pname)
                else TyCon tl (UnQual t2 recu)
        parametType _ t = t
        match (Ident _ n1) (Ident _ n2) = n1 == n2
        match _ _ = False
parametConstructor _ _ c = c 


-- TODO: Add deriving functor in tuple of one derive, not in list. 
-- (If we want user to be able to add deriving clauses)
-- Gives this error message for multiple deriving clauses:
-- Illegal use of multiple, consecutive deriving clauses
-- Use DerivingStrategies to allow this

-- | Create a Deriving functor for a given data type
deriveFunctor :: l -> Deriving l 
deriveFunctor l =
  Deriving l Nothing
    [IRule l Nothing 
      Nothing
      (IHCon l (UnQual l (Ident l "Functor")))]



-- | Get a name for the parametrized variable.            
-- TODO: Figure out how to handle unique names for parametrize variables
getParName :: l -> Name l 
getParName info = Ident info "a"


-- | Template Haskell derive for a piece
deriveTHPiece :: Name l -> Decl l 
deriveTHPiece pieceName = deriveTH pieceName ["makeTraversable", "makeFoldable", "makeEqF",
                                  "makeShowF", "smartConstructors", "smartAConstructors"]                                

-- | Template Haskell derive for a certain data type from a list of things to derive
deriveTH :: Name l -> [String] -> Decl l 
deriveTH targetName list = SpliceDecl l 
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
                                targetName
                            )
                        ]
                    )
                )
            )
        )
    where l = ann targetName


-- | Element for a thing to derive with Template Haskell
deriveTHListElem :: l -> String -> Exp l
deriveTHListElem l nam = Var l (UnQual l (Ident l nam))


-- TODO: derive liftSum for the class for a function/algebra


-- TODO: possibly more pragmas?
-- | Modify a list of pragmas to remove ComposableTypes and add DeriveFunctor, TemplateHaskell
modifyPragmas :: l -> [ModulePragma l] -> [ModulePragma l]
modifyPragmas l ps =  concatMap (addPragma l (removeCompTypes ps)) 
                                ["DeriveFunctor","TemplateHaskell","TypeOperators"] 
    where  
        addPragma :: l -> [ModulePragma l] -> String -> [ModulePragma l]
        addPragma l1 prs nam = if pragmasContain nam prs 
                                 then prs
                                 else (LanguagePragma l1 [Ident l1 nam]):prs
        removeCompTypes = filter (not . matchPragma "ComposableTypes")

-- | Check if the list of pragmas contain a certain one
pragmasContain :: String -> [ModulePragma l] -> Bool
pragmasContain nam = any (matchPragma nam)
        
-- | Check if a pragma match the given String
matchPragma :: String -> ModulePragma l -> Bool
matchPragma s (LanguagePragma _ [Ident _ nam]) = nam == s
matchPragma _ _ = False

coprod :: [Name l] -> Type l
coprod [nam] = TyCon l (UnQual l nam)
    where l = ann nam
coprod (nam:ns) = TyInfix l (TyCon l (UnQual l nam)) 
                                        (UnpromotedName l (UnQual l (Symbol l ":+:")))
                                        (coprod ns)
    where l = ann nam
coprod _ = undefined

