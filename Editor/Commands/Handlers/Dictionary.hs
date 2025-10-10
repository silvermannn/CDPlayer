{-# LANGUAGE OverloadedStrings #-}
module Editor.Commands.Handlers.Dictionary where

import CDDB.Parsers.OpenCorporaDict
import CDDB.Parsers.CoNLLU

import Editor.State
import Editor.Commands.Types

cmdLoadDictionary :: CommandHandler
cmdLoadDictionary state [] (CRStringList files) = do
    res <- parseText files
    case res of
        Left err -> return $ Left err
        Right dict -> return $ Right state {currentDictionary = Just dict}
cmdLoadDictionary _ _ _ = undefined

cmdLoadCoNLLU :: CommandHandler
cmdLoadCoNLLU state [] (CRStringList files) = do
    case currentDictionary state of
        Nothing -> return $ Left "No dictionary loaded"
        Just d -> do
            res <- parseCoNLLU d files
            case res of
                Left err -> return $ Left err
                Right trees -> return $ Right state {currentTrees = trees}
cmdLoadCoNLLU _ _ _ = undefined
