module Editor.Commands.Handlers.SaveLoad where

import Editor.State
import Editor.Commands.Types

import Support.Support

cmdSaveSentences :: CommandHandler
cmdSaveSentences state [CAString path] CRNothing = do
    res <- saveSentences (supportEngine state) path
    return $ Right state

cmdLoadSentences :: CommandHandler
cmdLoadSentences state [CAString path] CRNothing = do
    res <- loadSentences (supportEngine state) path
    return $ Right state

cmdSaveEncoder :: CommandHandler
cmdSaveEncoder state [CAString path] CRNothing = do
    res <- saveEncoder (supportEngine state) path
    return $ Right state

cmdLoadEncoder :: CommandHandler
cmdLoadEncoder state [CAString path] CRNothing = do
    res <- loadEncoder (supportEngine state) path
    return $ Right state

cmdSaveTagger :: CommandHandler
cmdSaveTagger state [CAString path] CRNothing = do
    res <- saveTagger (supportEngine state) path
    return $ Right state

cmdLoadTagger :: CommandHandler
cmdLoadTagger state [CAString path] CRNothing = do
    res <- loadTagger (supportEngine state) path
    return $ Right state

cmdSaveTreeBuilder :: CommandHandler
cmdSaveTreeBuilder state [CAString path] CRNothing = do
    res <- saveTreeBuilder (supportEngine state) path
    return $ Right state

cmdLoadTreeBuilder :: CommandHandler
cmdLoadTreeBuilder state [CAString path] CRNothing = do
    res <- loadTreeBuilder (supportEngine state) path
    return $ Right state
