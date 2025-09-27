module CDDB.Dictionary.Dictionary where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import Data.Text
import Data.Maybe (fromJust)

import CDDB.Dictionary.BidirectionalMap

data Dictionary = Dictionary {
        wordsCollection :: !(BidirectionalMap Text),
        tagsCollection :: !(BidirectionalMap Text),
        wordTags :: !(V.Vector [Text])
    }
    deriving Show

newDictionary m = Dictionary {
    wordsCollection = wc,
    tagsCollection = tc,
    wordTags = V.accum (++) ts $ fmap (\(a, b) -> (fromJust (findItem wc a), b)) $ M.toList m
    }
    where
        wc = fromSet $ M.keysSet m
        tc = fromSet $ S.unions $ fmap S.fromList $ M.elems m
        ts = V.replicate (size wc) mempty

