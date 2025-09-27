{-# LANGUAGE OverloadedStrings #-}
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
        tagsCollection :: !(BidirectionalMap Tag),
        wordTags :: !(V.Vector (S.Set Int)),
        unknownWordId :: Int
    }
    deriving Show

unknownWord :: Text
unknownWord = "<unknown>"

newDictionary :: M.Map Text TagSet -> Dictionary
newDictionary m = Dictionary {
    wordsCollection = wc,
    tagsCollection = tc,
    wordTags = V.accum S.union ts $ fmap (bimap (fromJust . findItem wc) (S.map (fromJust . findItem tc))) (M.toList m'),
    unknownWordId = fromJust $ findItem wc unknownWord
    }
    where
        wc = fromSet $ M.keysSet m'
        tc = fromSet $ S.unions $ M.elems m'
        ts = V.replicate (size wc) S.empty
        m' = M.insert unknownWord S.empty m

findWord :: Dictionary -> Text -> Maybe (Int, S.Set Int)
findWord d w = do
    wordId <- findItem (wordsCollection d) w
    return (wordId, wordTags d V.! wordId)

wordByIndex :: Dictionary -> Int -> Text
wordByIndex d i = lookupId (wordsCollection d) i

tagByIndex :: Dictionary -> Int -> Tag
tagByIndex d i = lookupId (tagsCollection d) i
