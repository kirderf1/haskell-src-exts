{-# LANGUAGE ComposableTypes #-}

module AltTyVarInExt where

piececategory A

data piece A ==> B = C

d -: A -> e

ext (Alternative f) => d @(f g) for B where
    d C = empty

