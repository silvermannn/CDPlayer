module Editor.Commands.Handlers.Train where

import Editor.Commands.Types

import Support.Support

cmdTrainTagger :: CommandHandler
cmdTrainTagger state [CAFloat sf] CRNothing = do
    _ <- trainTagger sf
    return $ Right state
cmdTrainTagger _ _ _ = undefined

cmdTrainTreeBuilder :: CommandHandler
cmdTrainTreeBuilder state [CAFloat sf] CRNothing = do
   _ <- trainTreeBuilder sf
   return $ Right state
cmdTrainTreeBuilder _ _ _ = undefined
