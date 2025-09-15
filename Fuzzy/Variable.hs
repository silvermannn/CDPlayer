module Fuzzy.Variable where

import Fuzzy.Function

class Function v a b => FuzzyVariable v a b where
    not :: v a b -> v a b
    or :: v a b -> v a b -> v a b
    and :: v a b -> v a b -> v a b

    valueAt :: a -> v a b -> b
    valueAt a v = apply a v 
