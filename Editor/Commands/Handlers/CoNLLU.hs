module Editor.Commands.Handlers.CoNLLU where

import Editor.State
import Editor.Commands.Types

import Support.Support

cmdParseCoNLLU :: CommandHandler
cmdParseCoNLLU state [] (CRStringList sentence) = do
    tags <- mapM (parsePath (supportEngine state) "CoNLLU") sentence
    print tags
    return $ Right state
