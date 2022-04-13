{-# LANGUAGE ComposableTypes #-}

module EmptyCheckInFunc where

d :: for (). a -> a
d a = a
