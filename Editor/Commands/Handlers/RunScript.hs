module Editor.Commands.Handlers.RunScript where

import Data.List (isPrefixOf)
import System.IO
import Control.Monad (foldM)

import Editor.Commands.Types
import Editor.Commands.Handlers

cmdRunScript :: CmdDescrs -> CommandHandler
cmdRunScript ds state [CAString path] CRNothing = do
    handle <- openFile path ReadMode
    fileContent <- hGetContents handle
    putStrLn "\rStarting..."
    state' <- foldM runLine (Right state) $ filter (not . ("#" `isPrefixOf`)) $ filter (not . null) $ lines fileContent
    putStrLn "\rDone."
    return state'
    where
        runLine (Right st) line = do
            putStr $ "\rRunning: " ++ line
            runInput st (words line) ds
        runLine l _ = return l
cmdRunScript _ _ _ _ = undefined
