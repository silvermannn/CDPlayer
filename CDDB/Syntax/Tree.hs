module CDDB.Syntax.Tree where

import qualified Data.Map as M

import Data.Tree

type POSTag = Int
type Feature = Int
type DependencyRelation = Int

data Tag = Tag POSTag [Feature] deriving (Eq, Show)

data DependencyTree = DependencyTree Tag (M.Map DependencyRelation DependencyTree) deriving Show

insertNode :: DependencyRelation -> DependencyTree -> DependencyTree -> DependencyTree
insertNode r d (DependencyTree a ch) = DependencyTree a $ M.insert r d ch

insertTag :: DependencyRelation -> Tag -> DependencyTree -> DependencyTree
insertTag r t (DependencyTree a ch) = DependencyTree a $ M.insert r (DependencyTree t M.empty) ch

findAllAndModifyTrees :: (Tag -> Bool) -> (DependencyTree -> DependencyTree) -> DependencyTree -> [DependencyTree]
findAllAndModifyTrees m f t@(DependencyTree a ch) = [f t | m a] ++ map (DependencyTree a) children
    where
        children = [M.insert k d ch | (k, v) <- M.toList ch, d <- findAllAndModifyTrees m f v]

scoreDifference :: DependencyTree -> DependencyTree -> Int
scoreDifference (DependencyTree a1 ch1) (DependencyTree a2 ch2) = undefined

toTree :: DependencyRelation -> DependencyTree -> Tree (DependencyRelation, Tag)
toTree root dt = go (root, dt)
    where
        go (root, DependencyTree a ch) = Node (root, a) $ map go $ M.toList ch

showDependencyTree root dt = putStrLn $ drawTree $ fmap show $ toTree root dt

tt = DependencyTree (Tag 3 []) (M.fromList [(100, DependencyTree (Tag 1 []) (M.fromList [])), (200, DependencyTree (Tag 2 []) (M.fromList []))])

