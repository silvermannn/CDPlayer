module Editor.Commands.Handlers.Parse where

import Editor.Commands.Types

import Support.Support

cmdParse :: String -> CommandHandler
cmdParse parser state [] (CRStringList sentence) = do
    res <- mapM (flip parsePath parser) sentence
    if and res 
        then return $ Right state
        else return $ Left $ show res
cmdParse _ _ _ _ = undefined
