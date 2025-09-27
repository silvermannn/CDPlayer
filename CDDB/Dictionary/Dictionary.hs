module CDDB.Dictionary.Dictionary where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Strict as V
import Data.Text
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)

import CDDB.Dictionary.BidirectionalMap

import CDDB.Syntax.Tag

data Dictionary = Dictionary {
        wordsCollection :: !(BidirectionalMap Text),
        tagsCollection :: !(BidirectionalMap (Tag Int)),
        wordTags :: !(V.Vector (S.Set Int))
    }
    deriving Show

newDictionary :: M.Map Text (S.Set (Tag Int)) -> Dictionary
newDictionary m = Dictionary {
    wordsCollection = wc,
    tagsCollection = tc,
    wordTags = V.accum S.union ts $ fmap (bimap (fromJust . findItem wc) (S.map (fromJust . findItem tc))) (M.toList m)
    }
    where
        wc = fromSet $ M.keysSet m
        tc = fromSet $ S.unions $ M.elems m
        ts = V.replicate (size wc) S.empty

findWord :: Dictionary -> Text -> Maybe (Int, S.Set Int)
findWord d w = do
    wordId <- findItem (wordsCollection d) w
    return (wordId, wordTags d V.! wordId)
