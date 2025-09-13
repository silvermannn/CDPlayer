module Editor.Commands.Handlers.Train where

import Data.Maybe (fromMaybe)

import Editor.State
import Editor.Commands.Types
import Editor.Commands.Handlers

import Support.Support

cmdTrainTagger :: CommandHandler
cmdTrainTagger state [CAFloat sf] CRNothing = do
    trainTagger (supportEngine state) sf
    return $ Right state
