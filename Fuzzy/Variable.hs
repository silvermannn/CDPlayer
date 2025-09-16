module Fuzzy.Variable where

import Fuzzy.Function.Function

class (Function v a b, Num b, Ord b) => FuzzyVariable v a b where
    not :: v a b -> v a b
    not v = transform (1-) v

    or :: v a b -> v a b -> v a b
    or v1 v2 = combine max v1 v2

    and :: v a b -> v a b -> v a b
    and v1 v2 = combine min v1 v2

    valueAt :: a -> v a b -> b
    valueAt a v = apply a v
