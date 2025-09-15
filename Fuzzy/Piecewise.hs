module Fuzzy.Variable where

import Data.List (union, zipWith4)
import Data.List.Extra (drop1)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

import Fuzzy.Function

data Piecewise f a b = Piecewise (f a b) (M.Map a (f a b))

lookupFunction a (Piecewise f m) = case S.lookupGE a (M.keysSet m) of
    Nothing -> f
    Just a' -> fromJust $ M.lookup a' m

instance (Ord a, Function f a b) => Function (Piecewise f) a b where
    apply a f = apply a $ lookupFunction a f
    intersects a1 a2 f1 f2 = undefined
    combine f p1@(Piecewise f1 m1) p2@(Piecewise f2 m2) = Piecewise (combine f f1 f2) m
        where
            ks = S.union (M.keysSet m2) (M.keysSet m2)
            as = S.toAscList ks
            fs1 = map (flip lookupFunction p1) as
            fs2 = map (flip lookupFunction p2) as
            iss = map fst $ concat (zipWith4 intersects as (drop1 as) fs1 fs2)
            as' = as ++ iss
            m = M.fromList $ zip as' $ zipWith (combine f) (map (flip lookupFunction p1) as') (map (flip lookupFunction p1) as')
