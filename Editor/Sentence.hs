{-# LANGUAGE OverloadedStrings #-}
module Editor.Sentence where

import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Bifunctor (bimap)

import CDDB.Syntax.Tag
import CDDB.Syntax.DependencyTree
import CDDB.Syntax.TreeBuilder
import CDDB.Dictionary.Dictionary

data CurrentSentence = CurrentSentence {
        original :: T.Text,
        tokenized :: [T.Text],
        wordIDsAndTags :: Maybe [(Int, S.Set Int)]
    }
    deriving Show

newSentence :: T.Text -> CurrentSentence
newSentence ss = CurrentSentence {
        original = ss,
        tokenized = T.words $ T.toLower ss,
        wordIDsAndTags = Nothing
    }

showSentence :: Maybe Dictionary -> (Int, CurrentSentence) -> IO ()
showSentence md (n, cs) = do
    TIO.putStrLn $ "#" <> T.pack (show n)
    TIO.putStrLn $ "Source sentence:\t\"" <> original cs <> "\""
    TIO.putStrLn $ "Tokenized sentence:\t[" <> T.intercalate " | " (tokenized cs) <> "]"
    TIO.putStrLn $ "Word IDs and tags:\t[" <> T.intercalate "," (map (T.pack . show) (fromMaybe [] $ wordIDsAndTags cs)) <> "]"
    case md of
        Nothing -> putStrLn "No dictionary loaded"
        Just d -> do
            TIO.putStrLn $ "\n" <> T.intercalate "\n" (map describeWord (fromMaybe [] $ wordIDsAndTags cs)) <> "\n"
            where
                describeWord (w, ts) = wordByIndex d w <> ":\n\t" <> T.intercalate "\n\t" (S.toList $ S.map (describeTag . tagByIndex d) ts)

tagSentence :: Dictionary -> CurrentSentence -> IO CurrentSentence
tagSentence d cs = do
    return $ cs {wordIDsAndTags = Just widsAndTags}
    where
        widsAndTags = map (fromMaybe (unknownWordId d, S.empty)) $ map (findWord d) (tokenized cs)

buildTree :: BuildRuleSet -> CurrentSentence -> IO CurrentSentence
buildTree rs cs = undefined
