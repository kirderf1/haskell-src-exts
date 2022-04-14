{-# LANGUAGE ComposableTypes #-}

module ContextInExt where

piececategory A

data piece A ==> B = C

d -: A -> e -> f

ext d @Int for B where
    d C = 1
