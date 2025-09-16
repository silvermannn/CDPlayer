module Editor.Commands.Handlers.Sentence where

import Data.Maybe (fromMaybe, fromJust)
import Data.List.Extra (chunksOf)

import Editor.State
import Editor.Commands.Types

import CDDB.Syntax.DependencyTree

import Support.Support

cmdTagSentence :: CommandHandler
cmdTagSentence state [] (CRStringList sentence) = do
    tags <- tag sentence
    print tags
    return $ Right state {taggedSentence = tags}
    where
        fineTag ix = undefined

cmdDescribeTags :: CommandHandler
cmdDescribeTags state [] (CRIntList tags) = do
    tagDescrs <- mapM (describeTag) tags
    print tagDescrs
    return $ Right state

cmdDescribeCurrentSentence :: CommandHandler
cmdDescribeCurrentSentence state [] CRNothing = case (taggedSentence state) of
    Nothing -> return $ Left "No current sentence tagged yet."
    Just tags -> do
        tagDescrs <- mapM (describeTag) tags
        print tagDescrs
        return $ Right state {taggedSentenceDescription = Just $ map (fromMaybe ["?"]) tagDescrs}

cmdShowCurrentSentence :: CommandHandler
cmdShowCurrentSentence state [] CRNothing = case (taggedSentence state) of
    Nothing -> return $ Left "No current sentence tagged yet."
    Just tags -> do
        print tags
        print $ taggedSentenceDescription state
        return $ Right state
