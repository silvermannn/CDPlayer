module Editor.Commands.Handlers.SaveLoad where

import Editor.Commands.Types

import Support.Support

cmdSaveSentences :: CommandHandler
cmdSaveSentences state [CAString path] CRNothing = do
    res <- saveSentences path
    return $ Right state

cmdLoadSentences :: CommandHandler
cmdLoadSentences state [CAString path] CRNothing = do
    res <- loadSentences path
    return $ Right state

cmdSaveEncoder :: CommandHandler
cmdSaveEncoder state [CAString path] CRNothing = do
    res <- saveEncoder path
    return $ Right state

cmdLoadEncoder :: CommandHandler
cmdLoadEncoder state [CAString path] CRNothing = do
    res <- loadEncoder path
    return $ Right state

cmdSaveTagger :: CommandHandler
cmdSaveTagger state [CAString path] CRNothing = do
    res <- saveTagger path
    return $ Right state

cmdLoadTagger :: CommandHandler
cmdLoadTagger state [CAString path] CRNothing = do
    res <- loadTagger path
    return $ Right state

cmdSaveTreeBuilder :: CommandHandler
cmdSaveTreeBuilder state [CAString path] CRNothing = do
    res <- saveTreeBuilder path
    return $ Right state

cmdLoadTreeBuilder :: CommandHandler
cmdLoadTreeBuilder state [CAString path] CRNothing = do
    res <- loadTreeBuilder path
    return $ Right state
