module Editor.Commands.Handlers.CoNLLU where

import Editor.Commands.Types

import Support.Support

cmdParseCoNLLU :: CommandHandler
cmdParseCoNLLU state [] (CRStringList sentence) = do
    tags <- mapM (flip parsePath "CoNLLU") sentence
    print tags
    return $ Right state
cmdParseCoNLLU _ _ _ = undefined
