module Fuzzy.Function.Function where

class Function f a b where
    domain :: f a b -> (a, a)
    apply :: a -> f a b -> b
    intersects :: a -> a -> f a b -> f a b -> [(a, b)]
    transform :: (b -> b) -> f a b -> f a b
    combine :: (b -> b -> b) -> f a b -> f a b -> f a b
