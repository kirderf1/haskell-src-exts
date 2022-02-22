module Types where

import Language.Haskell.Exts.Syntax

class TypeMap a where
    mapType :: (Type l -> Type l) -> a l -> a l

instance TypeMap Module where
    mapType f (Module l mmh ops iss dcls) =
          Module l mmh ops iss (mapType f <$> dcls)
    mapType f (XmlPage l mn os xn xas me es) =
          XmlPage l mn os xn (mapType f <$> xas) (mapType f <$> me) (mapType f <$> es)
    mapType f (XmlHybrid l mmh ops iss dcls xn xas me es) =
          XmlHybrid l mmh ops iss (mapType f <$> dcls) xn (mapType f <$> xas) (mapType f <$> me) (mapType f <$> es)

instance TypeMap Decl where
    mapType f decl = case decl of
            TypeDecl     l dh t      -> TypeDecl    l (mapType f dh) (mapType f t)
            TypeFamDecl  l dh mk mi  -> TypeFamDecl l (mapType f dh) (mapType f <$> mk) mi
            ClosedTypeFamDecl  l dh mk mi eqns  -> ClosedTypeFamDecl l (mapType f dh) (mapType f <$> mk) mi (mapType f <$> eqns)
            DataDecl     l dn mcx dh cds ders ->
                DataDecl l dn (mapType f <$> mcx) (mapType f dh) (mapType f <$> cds) (mapType f <$> ders)
            GDataDecl    l dn mcx dh mk gds ders ->
                GDataDecl l dn (mapType f <$> mcx) (mapType f dh) (mapType f <$> mk) (mapType f <$> gds) (mapType f <$> ders)
            DataFamDecl  l mcx dh mk         -> DataFamDecl l (mapType f <$> mcx) (mapType f dh) (mapType f <$> mk)
            TypeInsDecl  l t1 t2             -> TypeInsDecl l (mapType f t1) (mapType f t2)
            DataInsDecl  l dn t cds ders     -> DataInsDecl l dn (mapType f t) (mapType f <$> cds) (mapType f <$> ders)
            GDataInsDecl l dn t mk gds ders  -> GDataInsDecl l dn (mapType f t) (mapType f <$> mk) (mapType f <$> gds) (mapType f <$> ders)
            ClassDecl    l mcx dh fds cds    -> ClassDecl l (mapType f <$> mcx) (mapType f dh) fds ((mapType f <$>) <$> cds)
            InstDecl     l mo ih ids         -> InstDecl l mo (mapType f ih) ((mapType f <$>) <$> ids)
            DerivDecl    l mds mo ih         -> DerivDecl l (mapType f <$> mds) mo (mapType f ih)
            InfixDecl    l a k ops           -> InfixDecl l a k ops
            DefaultDecl  l ts                -> DefaultDecl l (mapType f <$> ts)
            SpliceDecl   l sp                -> SpliceDecl l (mapType f sp)
            TSpliceDecl  l sp                -> TSpliceDecl l (mapType f sp)
            TypeSig      l ns t              -> TypeSig l ns (mapType f t)
            PatSynSig    l n dh c1 dh2 c2 t  -> PatSynSig l n ((mapType f <$>) <$> dh) (mapType f <$> c1) ((mapType f <$>) <$> dh2) (mapType f <$> c2) (mapType f t)
            FunBind      l ms                -> FunBind l (mapType f <$> ms)
            PatBind      l p rhs bs          -> PatBind l (mapType f p) (mapType f rhs) (mapType f <$> bs)
            PatSyn           l p r d         -> PatSyn l (mapType f p) (mapType f r) (mapType f d)
            ForImp       l cc msf s n t      -> ForImp l cc msf s n (mapType f t)
            ForExp       l cc     s n t      -> ForExp l cc     s n (mapType f t)
            RulePragmaDecl   l rs            -> RulePragmaDecl l (mapType f <$> rs)
            DeprPragmaDecl   l nss           -> DeprPragmaDecl l nss
            WarnPragmaDecl   l nss           -> WarnPragmaDecl l nss
            InlineSig        l b act qn      -> InlineSig l b act qn
            InlineConlikeSig l   act qn      -> InlineConlikeSig l act qn
            SpecSig          l   act qn ts   -> SpecSig       l   act qn (mapType f <$> ts)
            SpecInlineSig    l b act qn ts   -> SpecInlineSig l b act qn (mapType f <$> ts)
            InstSig          l ih            -> InstSig l (mapType f ih)
            AnnPragma        l ann'          -> AnnPragma l (mapType f ann')
            MinimalPragma    l b             -> MinimalPragma l b
            RoleAnnotDecl    l t rs          -> RoleAnnotDecl l t rs
            CompletePragma   l cs ty         -> CompletePragma l cs ty
            PieceDecl   l ca dh cds ders     -> PieceDecl l ca dh (mapType f <$> cds) (mapType f <$> ders)
            CompFunDef  l ns t               -> CompFunDef l ns (mapType f t)

instance TypeMap PatternSynDirection where
    mapType _ Unidirectional                 = Unidirectional
    mapType _ ImplicitBidirectional          = ImplicitBidirectional
    mapType f (ExplicitBidirectional l dcls) = ExplicitBidirectional l (mapType f <$> dcls)

instance TypeMap TypeEqn where
    mapType f (TypeEqn l a b) = TypeEqn l (mapType f a) (mapType f b)

instance TypeMap Annotation where
    mapType f (Ann     l n e) = Ann     l n (mapType f e)
    mapType f (TypeAnn l n e) = TypeAnn l n (mapType f e)
    mapType f (ModuleAnn l e) = ModuleAnn l (mapType f e)

instance TypeMap ResultSig where
    mapType f (KindSig l k) = KindSig l (mapType f k)
    mapType f (TyVarSig l tv) = TyVarSig l (mapType f tv)

instance TypeMap DeclHead where
    mapType _ (DHead l n)           = DHead l n
    mapType f (DHInfix l tva n)     = DHInfix l (mapType f tva) n
    mapType f (DHParen l dh)        = DHParen l (mapType f dh)
    mapType f (DHApp l dh t)        = DHApp l (mapType f dh) (mapType f t)

instance TypeMap InstRule where
    mapType f (IRule l mtv cxt qn) = IRule l ((mapType f <$>) <$> mtv) (mapType f <$> cxt) (mapType f qn)
    mapType f (IParen l ih)        = IParen l (mapType f ih)

instance TypeMap InstHead where
    mapType _ (IHCon l n)           = IHCon l n
    mapType f (IHInfix l tva n)     = IHInfix l (mapType f tva) n
    mapType f (IHParen l dh)        = IHParen l (mapType f dh)
    mapType f (IHApp l dh t)        = IHApp l (mapType f dh) (mapType f t)

instance TypeMap Deriving where
    mapType f (Deriving l mds ihs) = Deriving l (mapType f <$> mds) (mapType f <$> ihs)

instance TypeMap DerivStrategy where
    mapType _ (DerivStock l)    = DerivStock l
    mapType _ (DerivAnyclass l) = DerivAnyclass l
    mapType _ (DerivNewtype l)  = DerivNewtype l
    mapType f (DerivVia l t)    = DerivVia l (mapType f t)

instance TypeMap Binds where
    mapType f (BDecls  l decls) = BDecls l (mapType f <$> decls)
    mapType f (IPBinds l ibs)   = IPBinds l (mapType f <$> ibs)

instance TypeMap IPBind where
    mapType f (IPBind l ipn e) = IPBind l ipn (mapType f e)

instance TypeMap Match where
    mapType f (Match l n ps rhs bs) = Match l n (mapType f <$> ps) (mapType f rhs) (mapType f <$> bs)
    mapType f (InfixMatch l a n b rhs bs) = InfixMatch l (mapType f a) n (mapType f <$> b) (mapType f rhs) (mapType f <$> bs)

instance TypeMap QualConDecl where
    mapType f (QualConDecl l tvs cx cd) = QualConDecl l ((mapType f <$>) <$> tvs) (mapType f <$> cx) (mapType f cd)

instance TypeMap ConDecl where
    mapType f (ConDecl l n bts) = ConDecl l n (mapType f <$> bts)
    mapType f (InfixConDecl l ta n tb) = InfixConDecl l (mapType f ta) n (mapType f tb)
    mapType f (RecDecl l n fds) = RecDecl l n (mapType f <$> fds)

instance TypeMap FieldDecl where
    mapType f (FieldDecl l ns t) = FieldDecl l ns (mapType f t)

instance TypeMap GadtDecl where
    mapType f (GadtDecl l n t1 t2 t3 t4) = GadtDecl l n ((mapType f <$>) <$> t1) (mapType f <$> t2) ((mapType f <$>) <$> t3) (mapType f t4)

instance TypeMap ClassDecl where
    mapType f (ClsDecl    l d) = ClsDecl l (mapType f d)
    mapType f (ClsDataFam l mcx dh mk) = ClsDataFam l (mapType f <$> mcx) (mapType f dh) (mapType f <$> mk)
    mapType f (ClsTyFam   l dh mk mi) = ClsTyFam l (mapType f dh) (mapType f <$> mk) mi
    mapType f (ClsTyDef   l t ) = ClsTyDef l (mapType f t)
    mapType f (ClsDefSig  l n t) = ClsDefSig l n (mapType f t)

instance TypeMap InstDecl where
    mapType f idecl = case idecl of
        InsDecl   l d           -> InsDecl l (mapType f d)
        InsType   l t1 t2       -> InsType l (mapType f t1) (mapType f t2)
        InsData   l dn t    cds ders -> InsData  l dn (mapType f t)    (mapType f <$> cds) (mapType f <$> ders)
        InsGData  l dn t mk gds ders -> InsGData l dn (mapType f t) (mapType f <$> mk) (mapType f <$> gds) (mapType f <$> ders)

instance TypeMap Rhs where
     mapType f (UnGuardedRhs l e)     = UnGuardedRhs l (mapType f e)
     mapType f (GuardedRhss  l grhss) = GuardedRhss  l (mapType f <$> grhss)

instance TypeMap GuardedRhs where
     mapType f (GuardedRhs l ss e) = GuardedRhs l (mapType f <$> ss) (mapType f e)

instance TypeMap Type where
    mapType f t1 = f $ case t1 of
          TyForall l mtvs mcx t         -> TyForall l ((mapType f <$>) <$> mtvs) (mapType f <$> mcx) (mapType f t)
          TyStar  l                     -> TyStar l
          TyFun   l t1' t2              -> TyFun l (mapType f t1') (mapType f t2)
          TyTuple l b ts                -> TyTuple l b ((mapType f) <$> ts)
          TyUnboxedSum l s              -> TyUnboxedSum l ((mapType f) <$> s)
          TyList  l t                   -> TyList l (mapType f t)
          TyParArray  l t               -> TyParArray l (mapType f t)
          TyApp   l t1' t2              -> TyApp l (mapType f t1') (mapType f t2)
          TyVar   l n                   -> TyVar l n
          TyCon   l qn                  -> TyCon l qn
          TyParen l t                   -> TyParen l (mapType f t)
          TyInfix l ta qn tb            -> TyInfix l (mapType f ta) qn (mapType f tb)
          TyKind  l t k                 -> TyKind l (mapType f t) (mapType f k)
          TyPromoted l   p              -> TyPromoted l (mapType f p)
          TyEquals l a b                -> TyEquals l (mapType f a) (mapType f b)
          TySplice l s                  -> TySplice l (mapType f s)
          TyBang l b u t                  -> TyBang l b u (mapType f t)
          TyWildCard l n                -> TyWildCard l n
          TyQuasiQuote l n s            -> TyQuasiQuote l n s
          TyComp l c t                  -> TyComp l c t

instance TypeMap Promoted where
    mapType _ (PromotedInteger l int raw) = PromotedInteger l int raw
    mapType _ (PromotedString l str raw) = PromotedString l str raw
    mapType _ (PromotedCon l b qn)   = PromotedCon l b qn
    mapType f (PromotedList l b ps)  = PromotedList  l b (mapType f <$> ps)
    mapType f (PromotedTuple l ps) = PromotedTuple l (mapType f <$> ps)
    mapType _ (PromotedUnit l)     = PromotedUnit l

instance TypeMap TyVarBind where
    mapType f (KindedVar   l n k) = KindedVar   l n (mapType f k)
    mapType _ (UnkindedVar l n)   = UnkindedVar l n

instance TypeMap Context where
    mapType f (CxSingle l asst ) = CxSingle l (mapType f asst)
    mapType f (CxTuple  l assts) = CxTuple  l (mapType f <$> assts)
    mapType _ (CxEmpty l) = CxEmpty l

instance TypeMap Asst where
    mapType f asst = case asst of
        TypeA l t           -> TypeA l (mapType f t)
        IParam l ipn t      -> IParam l ipn (mapType f t)
        ParenA l a          -> ParenA l (mapType f a)

instance TypeMap Exp where
    mapType f e1 = case e1 of
        Var l qn        -> Var l qn
        OverloadedLabel l qn -> OverloadedLabel l qn
        IPVar l ipn     -> IPVar l ipn
        Con l qn        -> Con l qn
        Lit l lit       -> Lit l lit
        InfixApp l e1' qop e2    -> InfixApp l (mapType f e1') qop (mapType f e2)
        App l e1' e2    -> App l (mapType f e1') (mapType f e2)
        NegApp l e      -> NegApp l (mapType f e)
        Lambda l ps e   -> Lambda l (mapType f <$> ps) (mapType f e)
        Let l bs e      -> Let l (mapType f bs) (mapType f e)
        If l ec et ee   -> If l (mapType f ec) (mapType f et) (mapType f ee)
        MultiIf l alts -> MultiIf l (mapType f <$> alts)
        Case l e alts   -> Case l (mapType f e) (mapType f <$> alts)
        Do l ss         -> Do l (mapType f <$> ss)
        MDo l ss        -> MDo l (mapType f <$> ss)
        Tuple l bx es   -> Tuple l bx (mapType f <$> es)
        UnboxedSum l b a es -> UnboxedSum l b a (mapType f es)
        TupleSection l bx mes -> TupleSection l bx ((mapType f <$>) <$> mes)
        List l es       -> List l (mapType f <$> es)
        ParArray l es   -> ParArray l (mapType f <$> es)
        Paren l e       -> Paren l (mapType f e)
        LeftSection l e qop     -> LeftSection l (mapType f e) qop
        RightSection l qop e    -> RightSection l qop (mapType f e)
        RecConstr l qn fups     -> RecConstr l qn (mapType f <$> fups)
        RecUpdate l e  fups     -> RecUpdate l (mapType f e) (mapType f <$> fups)
        EnumFrom l e            -> EnumFrom l (mapType f e)
        EnumFromTo l ef et      -> EnumFromTo l (mapType f ef) (mapType f et)
        EnumFromThen l ef et    -> EnumFromThen l (mapType f ef) (mapType f et)
        EnumFromThenTo l ef eth eto -> EnumFromThenTo l (mapType f ef) (mapType f eth) (mapType f eto)
        ParArrayFromTo l ef et  -> ParArrayFromTo l (mapType f ef) (mapType f et)
        ParArrayFromThenTo l ef eth eto -> ParArrayFromThenTo l (mapType f ef) (mapType f eth) (mapType f eto)
        ListComp l e qss        -> ListComp l (mapType f e) (mapType f <$> qss)
        ParComp  l e qsss       -> ParComp  l (mapType f e) ((mapType f <$>) <$> qsss)
        ParArrayComp  l e qsss  -> ParArrayComp  l (mapType f e) ((mapType f <$>) <$> qsss)
        ExpTypeSig l e t        -> ExpTypeSig l (mapType f e) (mapType f t)
        VarQuote l qn           -> VarQuote l qn
        TypQuote l qn           -> TypQuote l qn
        BracketExp l br         -> BracketExp l (mapType f br)
        SpliceExp l sp          -> SpliceExp l (mapType f sp)
        QuasiQuote l sn se      -> QuasiQuote l sn se
        TypeApp l t             -> TypeApp l (mapType f t)

        XTag  l xn xas me es     -> XTag  l xn (mapType f <$> xas) (mapType f <$> me) (mapType f <$> es)
        XETag l xn xas me        -> XETag l xn (mapType f <$> xas) (mapType f <$> me)
        XPcdata l s              -> XPcdata l s
        XExpTag l e              -> XExpTag l (mapType f e)
        XChildTag l es           -> XChildTag l (mapType f <$> es)

        CorePragma l s e   -> CorePragma l s (mapType f e)
        SCCPragma  l s e   -> SCCPragma l s (mapType f e)
        GenPragma  l s n12 n34 e -> GenPragma l s n12 n34 (mapType f e)

        Proc            l p e  -> Proc l (mapType f p) (mapType f e)
        LeftArrApp      l e1' e2 -> LeftArrApp      l (mapType f e1') (mapType f e2)
        RightArrApp     l e1' e2 -> RightArrApp     l (mapType f e1') (mapType f e2)
        LeftArrHighApp  l e1' e2 -> LeftArrHighApp  l (mapType f e1') (mapType f e2)
        RightArrHighApp l e1' e2 -> RightArrHighApp l (mapType f e1') (mapType f e2)
        ArrOp           l e      -> ArrOp           l (mapType f e)

        LCase l alts -> LCase l (mapType f <$> alts)

instance TypeMap XAttr where
    mapType f (XAttr l xn e) = XAttr l xn (mapType f e)

instance TypeMap Bracket where
    mapType f (ExpBracket l e) = ExpBracket l (mapType f e)
    mapType f (TExpBracket l e) = TExpBracket l (mapType f e)
    mapType f (PatBracket l p) = PatBracket l (mapType f p)
    mapType f (TypeBracket l t) = TypeBracket l (mapType f t)
    mapType f (DeclBracket l ds) = DeclBracket l (mapType f <$> ds)

instance TypeMap Splice where
    mapType _ (IdSplice l s) = IdSplice l s
    mapType _ (TIdSplice l s) = TIdSplice l s
    mapType f (ParenSplice l e) = ParenSplice l (mapType f e)
    mapType f (TParenSplice l e) = TParenSplice l (mapType f e)

instance TypeMap Rule where
    mapType f (Rule l s act mrvs e1 e2) = Rule l s act ((mapType f <$>) <$> mrvs) (mapType f e1) (mapType f e2)

instance TypeMap RuleVar where
    mapType _ (RuleVar l n) = RuleVar l n
    mapType f (TypedRuleVar l n t) = TypedRuleVar l n (mapType f t)

instance TypeMap Pat where
    mapType f p1 = case p1 of
      PVar l n          -> PVar l n
      PLit l sg lit     -> PLit l sg lit
      PNPlusK l n k     -> PNPlusK l n k
      PInfixApp l pa qn pb  -> PInfixApp l (mapType f pa) qn (mapType f pb)
      PApp l qn ps      -> PApp l qn (mapType f <$> ps)
      PTuple l bx ps    -> PTuple l bx (mapType f <$> ps)
      PUnboxedSum l b a ps -> PUnboxedSum l b a (mapType f ps)
      PList l ps        -> PList l (mapType f <$> ps)
      PParen l p        -> PParen l (mapType f p)
      PRec l qn pfs     -> PRec l qn (mapType f <$> pfs)
      PAsPat l n p      -> PAsPat l n (mapType f p)
      PWildCard l       -> PWildCard l
      PIrrPat l p       -> PIrrPat l (mapType f p)
      PatTypeSig l p t  -> PatTypeSig l (mapType f p) (mapType f t)
      PViewPat l e p    -> PViewPat l (mapType f e) (mapType f p)
      PRPat l rps       -> PRPat l (mapType f <$> rps)
      PXTag l xn pxas mp ps -> PXTag  l xn (mapType f <$> pxas) (mapType f <$> mp) (mapType f <$> ps)
      PXETag l xn pxas mp   -> PXETag l xn (mapType f <$> pxas) (mapType f <$> mp)
      PXPcdata l s      -> PXPcdata l s
      PXPatTag l p      -> PXPatTag l (mapType f p)
      PXRPats  l rps    -> PXRPats  l (mapType f <$> rps)
      PSplice l sp      -> PSplice l (mapType f sp)
      PQuasiQuote l sn st   -> PQuasiQuote l sn st
      PBangPat l p          -> PBangPat l (mapType f p)

instance TypeMap PXAttr where
    mapType f (PXAttr l xn p) = PXAttr l xn (mapType f p)

instance TypeMap RPat where
    mapType f rp1 = case rp1 of
      RPOp l rp rop         -> RPOp l (mapType f rp) rop
      RPEither l rp1' rp2   -> RPEither l (mapType f rp1') (mapType f rp2)
      RPSeq l rps           -> RPSeq l (mapType f <$> rps)
      RPGuard l p ss        -> RPGuard l (mapType f p) (mapType f <$> ss)
      RPCAs l n rp          -> RPCAs l n (mapType f rp)
      RPAs l n rp           -> RPAs l n (mapType f rp)
      RPParen l rp          -> RPParen l (mapType f rp)
      RPPat l p             -> RPPat l (mapType f p)

instance TypeMap PatField where
    mapType f (PFieldPat l qn p) = PFieldPat l qn (mapType f p)
    mapType _ (PFieldPun l n) = PFieldPun l n
    mapType _ (PFieldWildcard l) = PFieldWildcard l

instance TypeMap Stmt where
    mapType f (Generator l p e) = Generator l (mapType f p) (mapType f e)
    mapType f (Qualifier l e)   = Qualifier l (mapType f e)
    mapType f (LetStmt l bs)    = LetStmt l (mapType f bs)
    mapType f (RecStmt l ss)    = RecStmt l (mapType f <$> ss)

instance TypeMap QualStmt where
    mapType f (QualStmt     l s) = QualStmt l (mapType f s)
    mapType f (ThenTrans    l e) = ThenTrans l (mapType f e)
    mapType f (ThenBy       l e1 e2) = ThenBy l (mapType f e1) (mapType f e2)
    mapType f (GroupBy      l e) = GroupBy l (mapType f e)
    mapType f (GroupUsing   l e) = GroupUsing l (mapType f e)
    mapType f (GroupByUsing l e1 e2) = GroupByUsing l (mapType f e1) (mapType f e2)

instance TypeMap FieldUpdate where
    mapType f (FieldUpdate l qn e) = FieldUpdate l qn (mapType f e)
    mapType _ (FieldPun l n)       = FieldPun l n
    mapType _ (FieldWildcard l)    = FieldWildcard l

instance TypeMap Alt where
    mapType f (Alt l p gs bs) = Alt l (mapType f p) (mapType f gs) (mapType f <$> bs)
