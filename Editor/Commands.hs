{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands where

import Editor.Commands.Types
import Editor.Commands.Handlers
import Editor.Commands.Handlers.Help
import Editor.Commands.Handlers.Sentence

commands :: CmdDescrs
commands = [
    CmdDescr ["help"]           []                   (CRDStringList "topic") (showHelp commands) "Help.",
    CmdDescr ["load","conllu"]  [CADFilePath "path"] CRDNothing              undefined           "Load CoNNLU file or directory.",
    CmdDescr ["tag","sentence"] []                   (CRDStringList "word")  tagSentence         "Tag sentence.",
    CmdDescr ["describe","tag"] []                   (CRDIntList "tag")      describeTags        "Describe tags.",
    CmdDescr ["quit"]           []                   CRDNothing              cmdQuit             "Quit program."
    ]

runMainCommand input state = runInput state (words input) commands

cmdQuit :: CommandHandler
cmdQuit = undefined
