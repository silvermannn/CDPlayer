module CDDB.Learning.Rules where

import qualified Data.Heap as Heap
import Data.List
import Data.List.Extra
import Data.Function

import System.Random

import CDDB.Learning.Genetic

data Rule = A | B | C deriving (Show, Eq, Ord, Bounded)

int2Rule :: Int -> Rule
int2Rule n | n `mod` 3 == 0 = A
int2Rule n | n `mod` 3 == 1 = B
int2Rule n | n `mod` 3 == 2 = C

newtype RuleSet = Rules [Rule] deriving (Show)

instance Specie RuleSet where
    newborn s = Rules $ map int2Rule $ takeWhile (>0) $ iterate (`div` 3) s
    mutate s (Rules rs) = case p `mod` 100 of
        1 -> Rules $ rs1 ++ drop 1 rs2
        3 -> Rules $ rs1 ++ [int2Rule (p `div` 3)] ++ rs2
        _ -> Rules $ rs
        where
            (p, q) = s `divMod` length rs
            (rs1, rs2) = splitAt q rs
    breed s (Rules rs1) (Rules rs2) = Rules $ zipWith3 (\f a b -> if f == 0 then a else b) (map (`mod` 2) $ rnd s) rs1 rs2

newtype P a = P [a] deriving Show

rnd s = map abs $ randoms $ mkStdGen s

instance Population P RuleSet where
    size (P rss) = length rss
    bestFit f n (P rss) = P $ take n $ sortOn f rss
    make s n = P $ take n $ map newborn $ rnd s
    step f (rs, P rss) = (rs''', P $ mutated1 ++ mutated2 ++ survived)
        where
            (P survived) = bestFit f (length rss `div` 2) (P rss)
            (svs1, svs2) = splitAt (length survived `div` 2) survived
            (s: rs') = rs
            children = zipWith (breed s) svs1 svs2
            (rs1, rs'') = splitAt (length children) rs'
            (rs2, rs''') = splitAt (length children) rs''
            mutated1 = zipWith mutate rs children
            mutated2 = zipWith mutate (drop (length children) rs) children

fit (Rules rs) = - length rs + (length $ filter (/= C) rs)

algo :: Int -> Int -> Int -> P RuleSet
algo s n iter = snd $ last $ take iter $ iterate (step fit) (rnd s, make s n)
