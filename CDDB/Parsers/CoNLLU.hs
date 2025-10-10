{-# LANGUAGE OverloadedStrings #-}

module CDDB.Parsers.CoNLLU where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import Data.Maybe (catMaybes)
import Data.List (null)
import Data.List.Extra (split, notNull)

import CDDB.Syntax.DependencyTree
import CDDB.Dictionary.Dictionary

parseCoNLLU :: Dictionary -> [FilePath] -> IO (Either T.Text [DependencyTree])
parseCoNLLU d paths = do
    fileContent <- mapM TIO.readFile paths
    return $ Right $ parseSentences d $ (concatMap T.lines fileContent)

parseSentences :: Dictionary -> [T.Text] -> [DependencyTree]
parseSentences d = catMaybes . map (parseSentence d) . split T.null . filter ("#" `T.isPrefixOf`)

parseSentence :: Dictionary -> [T.Text] -> Maybe DependencyTree
parseSentence d ls = mapM (parseWord d) ls >>= buildTree

buildTree :: [(Int, Int)] -> Maybe DependencyTree
buildTree = undefined

parseWord d l = do
    (wId, tags) <- findWord d word
    return (wId, 0)
    where
        (word:_) = T.splitOn "\t" l

parseTags = undefined
