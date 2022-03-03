{-# LANGUAGE ComposableTypes #-}

piececategory A

data piece A ==> B = B Int
data piece A ==> C = C
data piece A ==> D = D A

type E = A ==> (B, C, D)

e :: Int -> Maybe E
e = undefined

main = putStrLn $ "Det funkar!"
