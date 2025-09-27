module CDDB.Syntax.DependencyTree where

import qualified Data.Map as M
import Data.Tree

import CDDB.Syntax.Tag

type DependencyRelation = Int

data NodeInfo = NodeInfo Int Int Tag

data DependencyTree = DependencyTree {
        wordId :: Int,
        sentencePos :: Int,
        tag :: Tag,
        children :: (M.Map DependencyRelation DependencyTree)
    } deriving Show

insertNode :: DependencyRelation -> DependencyTree -> DependencyTree -> DependencyTree
insertNode r d dt = dt {children = M.insert r d (children dt)}

insertTag :: DependencyRelation -> Int -> Int -> Tag -> DependencyTree -> DependencyTree
insertTag r w pos t dt  = dt {children = M.insert r (DependencyTree w pos t M.empty) (children dt)}

findAllAndModifyTrees :: (Tag -> Bool) -> (DependencyTree -> DependencyTree) -> DependencyTree -> [(Int, Int, Tag, DependencyTree)]
findAllAndModifyTrees m f dt@(DependencyTree w pos t ch) = [(w, pos, t, f dt) | m t] ++ [(w', pos', t', DependencyTree w pos t ch') | (w', pos', t', ch') <- children]
    where
        children = [(w', pos', t', M.insert k d ch) | (k, v) <- M.toList ch, (w', pos', t', d) <- findAllAndModifyTrees m f v]

toTree :: DependencyRelation -> DependencyTree -> Tree (DependencyRelation, Int, Int, Tag)
toTree root dt = go (root, dt)
    where
        go (root, DependencyTree m w pos ch) = Node (root, m, w, pos) $ map go $ M.toList ch

showDependencyTree root dt = putStrLn $ drawTree $ fmap show $ toTree root dt
