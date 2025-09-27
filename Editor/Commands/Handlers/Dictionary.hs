module Editor.Commands.Handlers.Dictionary where

import CDDB.Parsers.OpenCorporaDict

import Editor.State
import Editor.Commands.Types

cmdLoadDictionary :: CommandHandler
cmdLoadDictionary state [] (CRStringList files) = do
    res <- parseText files
    case res of
        Left err -> return $ Left err
        Right dict -> return $ Right state {currentDictionary = Just dict}
cmdLoadDictionary _ _ _ = undefined
