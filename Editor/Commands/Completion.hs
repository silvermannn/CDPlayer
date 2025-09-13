module Editor.Commands.Completion where

import Data.List

import System.Console.Haskeline.Completion

import Editor.Commands
import Editor.Commands.Types
import Editor.Commands.Handlers

-- (String, String) -> m (String, [Completion])
editorComplete :: CompletionFunc IO
editorComplete (s1, s2) = if "./" `isPrefixOf` s1 || "/" `isPrefixOf` s1
    then completeFilename (s1, s2)
    else return ("", completions)
    where
        ws = words $ reverse s1
        descrs = filterCommandDescr commands ws
        completions = map makeCompletion $ filter ((>= length ws) . length . keywords) descrs
        makeCompletion d = Completion (unwords $ keywords d) (describeCommand d ++ "\t\t") True
