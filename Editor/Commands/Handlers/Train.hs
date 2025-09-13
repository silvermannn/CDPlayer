module Editor.Commands.Handlers.Train where

import Editor.State
import Editor.Commands.Types

import Support.Support

cmdTrainTagger :: CommandHandler
cmdTrainTagger state [CAFloat sf] CRNothing = do
    trainTagger (supportEngine state) sf
    return $ Right state
