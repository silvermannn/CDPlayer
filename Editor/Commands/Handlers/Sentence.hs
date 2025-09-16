module Editor.Commands.Handlers.Sentence where

import Data.Maybe (fromMaybe, fromJust)
import Data.List.Extra (chunksOf)

import Editor.State
import Editor.Commands.Types
import Editor.Support

import CDDB.Syntax.DependencyTree

cmdTagSentence :: CommandHandler
cmdTagSentence state [] (CRStringList sentence) = do
    tags <- tagSentence sentence
    print tags
    return $ Right state {currentTaggedSentence = tags}
    where
        fineTag ix = undefined

cmdDescribeTags :: CommandHandler
cmdDescribeTags state [] (CRIntList tags) = do
    --tagDescrs <- mapM (describeTag) tags
    --print tagDescrs
    return $ Right state

cmdDescribeCurrentSentence :: CommandHandler
cmdDescribeCurrentSentence state [] CRNothing = case (currentTaggedSentence state) of
    Nothing -> return $ Left "No current sentence tagged yet."
    Just tags -> do
        tagDescrs <- describeTags tags
        print tagDescrs
        return $ Right state {currentTaggedSentenceStr = tagDescrs}

cmdShowCurrentSentence :: CommandHandler
cmdShowCurrentSentence state [] CRNothing = case (currentTaggedSentence state) of
    Nothing -> return $ Left "No current sentence tagged yet."
    Just tags -> do
        print tags
        print $ currentTaggedSentenceStr state
        return $ Right state
