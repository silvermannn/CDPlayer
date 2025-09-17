module Fuzzy.Variable where

import Fuzzy.Function.Function

class (Function v a b, Num b, Ord b) => FuzzyVariable v a b where
    not :: v a b -> v a b
    not = transform (1-)

    or :: v a b -> v a b -> v a b
    or = combine max

    and :: v a b -> v a b -> v a b
    and = combine min

    valueAt :: a -> v a b -> b
    valueAt = apply
