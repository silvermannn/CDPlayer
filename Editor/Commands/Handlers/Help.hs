module Editor.Commands.Handlers.Help where

import Editor.Commands.Types
import Editor.Commands.Handlers

showHelp :: CmdDescrs -> CommandHandler
showHelp ds state [] (CRStringList []) = do
    putStrLn (unlines $ map describeCommandWHelp ds)
    return $ Right state
showHelp ds state [] (CRStringList cmds) = case filterCommandDescr ds cmds of
    [] -> do
        putStrLn $ "No help found for: " ++ unwords cmds
        return $ Right state
    hss -> do
        putStrLn (unlines $ map describeCommandWHelp hss)
        return $ Right state

describeCommandWHelp cmd = helpString cmd ++ "\n\t" ++ describeCommand cmd
