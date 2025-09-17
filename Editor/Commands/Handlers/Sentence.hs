module Editor.Commands.Handlers.Sentence where

import Data.Maybe (catMaybes)
import Data.List.Extra (chunksOf)

import CDDB.Utils

import Editor.State
import Editor.Commands.Types
import Editor.Sentence
import Editor.Utilites

cmdNewSentence :: CommandHandler
cmdNewSentence state [] (CRString sentence) = do
    return $ Right state {currentSentences = newSentence sentence: (currentSentences state)}

cmdShowSentences :: CommandHandler
cmdShowSentences state [] CRNothing = do
    mapM_ showSentence (currentSentences state)
    return $ Right state

cmdDeleteSentences :: CommandHandler
cmdDeleteSentences state [] (CRIntList ns) = if null ns
    then return $ Right state {currentSentences = []}
    else return $ Right state {currentSentences = deleteItemsByNumbers (currentSentences state) ns }

cmdTagSentence :: CommandHandler
cmdTagSentence state [] (CRIntList ns) = do
    tagged <- mapM tagSentence affected
    return $ Right state {currentSentences = passed ++ tagged}
    where
        (affected, passed) = filterByNs (currentSentences state) ns

cmdBuildSentenceTree :: CommandHandler
cmdBuildSentenceTree state [] (CRIntList ns) = do
    built <- mapM buidTree affected
    return $ Right state {currentSentences = passed ++ built}
    where
        (affected, passed) = filterByNs (currentSentences state) ns
