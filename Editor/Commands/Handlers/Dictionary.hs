module Editor.Commands.Handlers.Dictionary where

import CDDB.Parsers.OpenCorporaDict

import Editor.Commands.Types

cmdLoadDictionary :: CommandHandler
cmdLoadDictionary state [] (CRStringList files) = do
    res <- mapM parseText files
    print res
    return $ Right state
cmdLoadDictionary _ _ _ = undefined
