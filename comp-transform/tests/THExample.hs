{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

type Sig = Op :+: Value

$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Value, ''Op])
