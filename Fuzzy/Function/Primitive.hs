module Fuzzy.Function.Primitive where

import Fuzzy.Function.Function

data PrimitiveFunction a b =
      Const b
    | Linear a b
    deriving (Eq, Show)

instance (Num a, Fractional a, Ord a) => Function PrimitiveFunction a a where
    domain _ = (-inf, inf)

    apply _ (Const c) = c
    apply x (Linear k b) = k * x + b

    intersects _ _ (Const _) (Const _) = []
    intersects a1 a2 (Const c) (Linear k b) = [(x, c) | x > a1 && x < a2]
        where x = (c - b) / k
    intersects a1 a2 (Linear k1 b1) (Linear k2 b2) = [(x, k1 * x + b1) | x > a1 && x < a2]
        where x = (b2 - b1) / (k1 - k2)
    intersects a1 a2 f1 f2 = intersects a1 a2 f2 f1

    --ltransform t1 f = transform1 t1 f

--    combine f (Const c1) (Const c2) = Const $ f c1 c2
--    combine f (Const c1) (Linear k b) = Const $ f c1 c2

inf :: (Num a, Fractional a) => a
inf = 1/0