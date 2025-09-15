module CDDB.Syntax.Tag where

data Tag a = Tag a [(a, a)] deriving (Eq, Show)

