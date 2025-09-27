{-# LANGUAGE OverloadedStrings #-}
module CDDB.Syntax.Tag where

import Data.Text (Text, intercalate)

import CDDB.Dictionary.BidirectionalMap
import CDDB.Dictionary.UniversalDependencies

data Tag = Tag Int [(Int, Int)] deriving (Eq, Ord, Show)

type Tags = [Tag]

describeTag :: Tag -> Text
describeTag (Tag pos fs) = lookupId uPOSTags pos <> " (" <> intercalate ", " (map showFeature fs) <> ")"
    where
        showFeature (n, v) = lookupId featureNames n <> ": " <> lookupId featureValues v
