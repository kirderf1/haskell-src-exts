{-# LANGUAGE TemplateHaskell #-}

$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Value, ''Op])
