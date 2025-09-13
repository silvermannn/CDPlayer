module Editor.Commands.Handlers.CoNLLU where

import Data.Maybe (fromMaybe)

import Editor.State
import Editor.Commands.Types
import Editor.Commands.Handlers

import Support.Support

cmdParseCoNLLU :: CommandHandler
cmdParseCoNLLU state [] (CRStringList sentence) = do
    tags <- mapM (parsePath (supportEngine state) "CoNLLU") sentence
    print tags
    return $ Right state
