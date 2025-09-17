module Editor.Commands.Handlers.Sentence where

import Editor.State
import Editor.Commands.Types
import Editor.Sentence
import Editor.Utilites

cmdNewSentence :: CommandHandler
cmdNewSentence state [] (CRString sentence) = do
    return $ Right state {currentSentences = newSentence sentence: currentSentences state}
cmdNewSentence _ _ _ = undefined

cmdShowSentences :: CommandHandler
cmdShowSentences state [] CRNothing = do
    mapM_ showSentence (currentSentences state)
    return $ Right state
cmdShowSentences _ _ _ = undefined

cmdDeleteSentences :: CommandHandler
cmdDeleteSentences state [] (CRIntList ns) = if null ns
    then return $ Right state {currentSentences = []}
    else return $ Right state {currentSentences = deleteItemsByNumbers (currentSentences state) ns }
cmdDeleteSentences _ _ _ = undefined

cmdTagSentence :: CommandHandler
cmdTagSentence state [] (CRIntList ns) = do
    tagged <- mapM tagSentence affected
    return $ Right state {currentSentences = passed ++ tagged}
    where
        (affected, passed) = filterByNs (currentSentences state) ns
cmdTagSentence _ _ _ = undefined

cmdBuildSentenceTree :: CommandHandler
cmdBuildSentenceTree state [] (CRIntList ns) = do
    built <- mapM buidTree affected
    return $ Right state {currentSentences = passed ++ built}
    where
        (affected, passed) = filterByNs (currentSentences state) ns
cmdBuildSentenceTree _ _ _ = undefined
