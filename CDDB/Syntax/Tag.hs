module CDDB.Syntax.Tag where

import Data.List.Extra (intercalate)

data Tag a = Tag a [(a, a)] deriving (Eq)

type Tags a = [Tag a]

instance Show a => Show (Tag a) where
    show (Tag pos fs) = show pos ++ " (" ++ intercalate ", " (map s fs) ++ ")"
        where
            s (n, v) = show n ++ ": " ++ show v