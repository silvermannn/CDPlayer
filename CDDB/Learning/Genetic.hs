module CDDB.Learning.Genetic where

import System.Random

class Specie a where
    newborn :: Int -> a
    mutate :: Int -> a -> a
    breed :: Int -> a -> a -> a

class Specie b => Population a b where
    size :: a b -> Int
    make :: Int -> Int -> a b
    bestFit :: Ord n => (b -> n) -> Int -> a b -> a b
    step :: Ord n => (b -> n) -> ([Int], a b) -> ([Int], a b)
