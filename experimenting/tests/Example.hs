
{-# LANGUAGE ComposableTypes #-}

piece Testa a = Add a a

$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Value, ''Op])
