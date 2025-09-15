module Fuzzy.Function where

class Function f a b where
    apply :: a -> f a b -> b
    intersects :: a -> a -> f a b -> f a b -> [(a, b)]
    combine :: (b -> b -> b) -> f a b -> f a b -> f a b
