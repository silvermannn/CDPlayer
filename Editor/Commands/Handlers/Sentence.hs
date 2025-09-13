module Editor.Commands.Handlers.Sentence where

import Editor.State
import Editor.Commands.Types

import Support.Support

cmdTagSentence :: CommandHandler
cmdTagSentence state [] (CRStringList sentence) = do
    tags <- tag (supportEngine state) sentence
    print tags
    return $ Right state {taggedSentence = tags}

cmdDescribeTags :: CommandHandler
cmdDescribeTags state [] (CRIntList tags) = do
    tagDescrs <- mapM (describeTag (supportEngine state)) tags
    print tagDescrs
    return $ Right state
