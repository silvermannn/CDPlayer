module Fuzzy.Variable where

import Data.List (union, zipWith4)
import Data.List.Extra (drop1)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

import Fuzzy.Function

import Debug.Trace

-- Key to easy extract function in range by value in range
data Key a = Pair a a | Value a

first (Pair a b) = a

instance Show a => Show (Key a) where
    show (Pair a b) = show (a, b)
    show (Value a) = show a

instance Ord a => Eq (Key a) where
    (Pair a1 a2) == (Pair b1 b2) = a1 == b1 && a2 == b2
    (Pair a1 a2) == (Value b)    = a1 < b && b < a2
    (Value b)    == (Pair a1 a2) = a1 < b && b < a2
    (Value a)    == (Value b)    = a == b

instance Ord a => Ord (Key a) where
    compare (Pair a1 a2) (Pair b1 b2) = compare (a1, a2) (b1, b2)
    compare (Value a) (Value b) = compare a b
    compare (Pair a1 a2) (Value b) | b < a1 = GT
    compare (Pair a1 a2) (Value b) | b > a2 = LT
    compare (Pair a1 a2) (Value b) = EQ
    compare (Value a) (Pair b1 b2) | a < b1 = LT
    compare (Value a) (Pair b1 b2) | a > b2 = GT
    compare (Value a) (Pair b1 b2) = EQ

data Piecewise f a b = Piecewise (M.Map (Key a) (f a b))

instance (Ord a, Function f a b) => Function (Piecewise f) a b where
    domain (Piecewise m) = (first $ fst $ M.findMin m, first $ fst $ M.findMax m)
    apply a (Piecewise m) = apply a $ fromJust $ M.lookup (Value a) m
    intersects a1 a2 (Piecewise m1) (Piecewise m2) = mergePairs intersectKV (M.assocs m1) (M.assocs m2)
        where
            intersectKV (Pair a1 a2, f1) (p2, f2) = intersects a1 a2 f1 f2
    combine f (Piecewise m1) (Piecewise m2) = Piecewise $ M.fromList $ mergePairs combineKV (M.assocs m1) (M.assocs m2)
        where
            combineKV (Pair a1 a2, f1) (p2, f2) = [(p, combine f f1 f2) | p <- zipWith Pair as (drop1 as)]
                where
                    as = a1 : (map fst $ intersects a1 a2 f1 f2) ++ [a2]

inf :: Float
inf = 1/0

mergePairs _    []       []                          = []
mergePairs comb (l1:ls1) (l2:ls2) | fst l1 == fst l2 = comb l1 l2 ++ mergePairs comb ls1 ls2
mergePairs comb (l1:ls1) (l2:ls2)                    = comb p1 p2 ++ if second
        then mergePairs comb ls1 (pNew: ls2)
        else mergePairs comb (pNew: ls1) ls2
    where
        (p1, p2, pNew, second) = combinePairs l1 l2

        combinePairs ((Pair a1 a2), f1) ((Pair b1 b2), f2) | a1 == b1 && a2 < b2 = ((Pair a1 a2, f1), (Pair b1 a2, f2), (Pair a2 b2, f2), True)
        combinePairs ((Pair a1 a2), f1) ((Pair b1 b2), f2) | a1 == b1 && a2 > b2 = ((Pair a1 b2, f1), (Pair b1 b2, f2), (Pair b2 a2, f1), False)
