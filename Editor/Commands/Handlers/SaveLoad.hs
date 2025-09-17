module Editor.Commands.Handlers.SaveLoad where

import Editor.Commands.Types

import Support.Support

cmdSaveSentences :: CommandHandler
cmdSaveSentences state [CAString path] CRNothing = do
    _ <- saveSentences path
    return $ Right state
cmdSaveSentences _ _ _ = undefined

cmdLoadSentences :: CommandHandler
cmdLoadSentences state [CAString path] CRNothing = do
    _ <- loadSentences path
    return $ Right state
cmdLoadSentences _ _ _ = undefined

cmdSaveEncoder :: CommandHandler
cmdSaveEncoder state [CAString path] CRNothing = do
    _ <- saveEncoder path
    return $ Right state
cmdSaveEncoder _ _ _ = undefined

cmdLoadEncoder :: CommandHandler
cmdLoadEncoder state [CAString path] CRNothing = do
    _ <- loadEncoder path
    return $ Right state
cmdLoadEncoder _ _ _ = undefined

cmdSaveTagger :: CommandHandler
cmdSaveTagger state [CAString path] CRNothing = do
    _ <- saveTagger path
    return $ Right state
cmdSaveTagger _ _ _ = undefined

cmdLoadTagger :: CommandHandler
cmdLoadTagger state [CAString path] CRNothing = do
    _ <- loadTagger path
    return $ Right state
cmdLoadTagger _ _ _ = undefined

cmdSaveTreeBuilder :: CommandHandler
cmdSaveTreeBuilder state [CAString path] CRNothing = do
    _ <- saveTreeBuilder path
    return $ Right state
cmdSaveTreeBuilder _ _ _ = undefined

cmdLoadTreeBuilder :: CommandHandler
cmdLoadTreeBuilder state [CAString path] CRNothing = do
    _ <- loadTreeBuilder path
    return $ Right state
cmdLoadTreeBuilder _ _ _ = undefined
