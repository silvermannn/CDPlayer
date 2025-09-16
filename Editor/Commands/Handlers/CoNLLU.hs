module Editor.Commands.Handlers.CoNLLU where

import Editor.Commands.Types

import Support.Support

cmdParseCoNLLU :: CommandHandler
cmdParseCoNLLU state [] (CRStringList sentence) = do
    tags <- mapM (parsePath "CoNLLU") sentence
    print tags
    return $ Right state
