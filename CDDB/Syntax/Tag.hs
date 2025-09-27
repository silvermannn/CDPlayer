{-# LANGUAGE OverloadedStrings #-}
module CDDB.Syntax.Tag where

import qualified Data.Set as S
import Data.Text (Text, intercalate)

import CDDB.Dictionary.BidirectionalMap
import CDDB.Dictionary.UniversalDependencies

data Tag = Tag Int [(Int, Int)] deriving (Eq, Ord, Show)

type TagSet = S.Set Tag

describeTag :: Tag -> Text
describeTag (Tag pos fs) = lookupId uPOSTags pos <> " (" <> intercalate ", " (map showFeature fs) <> ")"
    where
        showFeature (n, v) = lookupId featureNames n <> ": " <> lookupId featureValues v
