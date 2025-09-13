module Editor.Commands.Handlers.Help where

import Editor.Commands.Types
import Editor.Commands.Handlers

showHelp :: CmdDescrs -> CommandHandler
showHelp ds state [] (CRStringList []) = do
    putStrLn (unlines $ map describeCommand ds)
    return $ Right state     
showHelp ds state [] (CRStringList cmds) = case filterCommandDescr ds cmds of
    [] -> do
        putStrLn $ "No help found for: " ++ unwords cmds
        return $ Right state
    hss -> do 
        putStrLn (unlines $ map describeCommand hss)
        return $ Right state

describeCommand (CmdDescr kw ma ra _ hs) = hs ++ "\n\t" ++ unwords kw ++ unwords (map describeCommandDef ma) ++ " " ++ describeCommandRest ra
    where
        describeCommandDef (CADString s) = "<" ++ s ++ ">"
        describeCommandDef (CADInt s) = "<int:" ++ s ++ ">"
        describeCommandDef (CADFloat s) = "<float:" ++ s ++ ">"

        describeCommandRest (CRDStringList s) = "<" ++ s ++ "> ... <" ++ s ++ ">"
        describeCommandRest (CRDString s) = "<" ++ s ++ ">"
        describeCommandRest (CRDIntList s) = "<int:" ++ s ++ "> ... <int:" ++ s ++ ">"
        describeCommandRest CRDNothing = ""
