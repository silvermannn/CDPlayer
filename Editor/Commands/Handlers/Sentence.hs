module Editor.Commands.Handlers.Sentence where

import Data.Maybe (fromMaybe)

import Editor.State
import Editor.Commands.Types
import Editor.Commands.Handlers

import Support.Support

tagSentence :: CommandHandler
tagSentence state [] (CRStringList sentence) = do
    tags <- tag (supportEngine state) sentence
    print tags
    return $ Right state {taggedSentence = tags}

describeTags :: CommandHandler
describeTags state [] (CRIntList tags) = do
    tagDescrs <- mapM (describeTag (supportEngine state)) tags
    print tagDescrs
    return $ Right state
