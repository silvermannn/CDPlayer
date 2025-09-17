module Editor.Commands.Handlers.Train where

import Editor.Commands.Types

import Support.Support

cmdTrainTagger :: CommandHandler
cmdTrainTagger state [CAFloat sf] CRNothing = do
    trainTagger sf
    return $ Right state

cmdTrainTreeBuilder :: CommandHandler
cmdTrainTreeBuilder state [CAFloat sf] CRNothing = do
    return $ Right state
