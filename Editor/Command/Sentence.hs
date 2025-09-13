{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Sentence where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Read

import Editor.Command.Types
import Editor.Command.Help
import Editor.Command.Errors

import Support.Support

sentenceCommands :: CommandMap
sentenceCommands = M.fromList [
        ("help",   CommandDef (cmdHelp sentenceCommands) "This help."),
        ("describeTag", CommandDef cmdDescribeTag "Describe tag <tag>"),
        ("showTags", CommandDef cmdShowSentence "Show tagged sentence."),
        ("tag", CommandDef cmdTagSentence "Tag sentence.")
    ]

cmdTagSentence :: Command
cmdTagSentence ns state = do
    tags <- tag (supportEngine state) ns
    print tags
    return $ Right state {taggedSentence = tags}
cmdTagSentence [] _ = return errNotEnoughArguments

cmdShowSentence :: Command
cmdShowSentence [] state = do
    tagDescrs <- mapM (describeTag (supportEngine state)) (fromMaybe [] $ taggedSentence state)
    mapM print tagDescrs
    return $ Right state
cmdShowSentence _ _ = return errTooManyArguments

cmdDescribeTag :: Command
cmdDescribeTag [n] state = case readEither n of
        Left err -> return $ Left err
        Right tag -> do
            tagDescrs <- describeTag (supportEngine state) tag
            print tagDescrs
            return $ Right state
cmdDescribeTag [] _ = return errNotEnoughArguments
cmdDescribeTag _ _ = return errTooManyArguments
