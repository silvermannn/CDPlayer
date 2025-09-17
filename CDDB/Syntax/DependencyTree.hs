module CDDB.Syntax.DependencyTree where

import CDDB.Syntax.Tag

data DependencyTree a = DTNode a (Tag a) [(a, DependencyTree a)] deriving (Eq, Show)
