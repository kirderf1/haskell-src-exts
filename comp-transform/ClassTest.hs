{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where
import qualified Data.Comp
import qualified Data.Comp.Derive
import qualified Data.Comp.Show ()
import qualified Data.Comp.Equality ()


data Val a = Const Int 
    deriving Functor
    
data Op a = Add a a | Mult a a 
    deriving Functor
    
data Sugar a = Neg a
    deriving Functor
    
$(
  Data.Comp.Derive.derive [Data.Comp.Derive.smartConstructors] [''Val,''Op,''Sugar]
  )  


type Sig = Data.Comp.Term (Val Data.Comp.:+: Op Data.Comp.:+: Sugar)

class Desug f a where
    desug' :: (Desug g a) => f (Data.Comp.Term g) ->  a 
    
desug :: (Desug g a) => Data.Comp.Term g ->  a
desug = desug'  . Data.Comp.unTerm

{-# NOINLINE desug #-}

$( Data.Comp.Derive.derive [Data.Comp.Derive.liftSum] [''Desug] )

instance PartOf Val a => Desug Val a where
        desug' (Const c) = iConst' c 

instance PartOf Op a => Desug Op a where
        desug' (Add e1 e2) = iAdd' (desug e1) (desug e2)
        desug' (Mult e1 e2) = iMult' (desug e1) (desug e2)

instance (PartOf Val a, PartOf Op a) =>
         Desug Sugar a
         where
        desug' (Neg e) = iConst' (-1) `iMult'` (desug e)
            --inject' $ Mult (inject' (Const (-1))) (desug e)
        
        
class PartOf a b where
    inject' :: a b -> b
    
instance (a Data.Comp.:<: b) => PartOf a (Data.Comp.Term b) where
    inject' = Data.Comp.inject
   
   
iConst' x = inject' $ Const x
iAdd' x y = inject' $ Add x y
iMult' x y = inject' $ Mult x y

  
    
class Eval f where
        eval' :: (Eval g) => f (Data.Comp.Term g) -> Int

eval :: (Eval g) => Data.Comp.Term g -> Int
eval = eval' . Data.Comp.unTerm

{-# NOINLINE eval #-}

$( Data.Comp.Derive.derive [Data.Comp.Derive.liftSum] [''Eval] )

instance Eval Val where
        eval' (Const i) = i

instance Eval Op where
        eval' (Add e1 e2) = eval e1 + eval e2
        eval' (Mult e1 e2) = eval e1 * eval e2
        
--evalEx :: Int
--evalEx = eval (expr :: Sig)

expr ::
       (Val Data.Comp.:<: a, Op Data.Comp.:<: a) => Data.Comp.Term a
expr = iConst 1 `iAdd` (iConst 2 `iMult` iConst 2)

sugarEx :: (Val Data.Comp.:<: a, Op Data.Comp.:<: a, Sugar Data.Comp.:<: a) => Data.Comp.Term a 
sugarEx = iNeg expr 

type Sig' = Data.Comp.Term (Val Data.Comp.:+: Op)

desugEx :: Int
desugEx = eval $ (desug (sugarEx :: Sig) :: Sig')

main = print desugEx

