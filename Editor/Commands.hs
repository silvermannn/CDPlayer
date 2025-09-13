{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands where

import Editor.Commands.Types
import Editor.Commands.Handlers
import Editor.Commands.Handlers.Help
import Editor.Commands.Handlers.CoNLLU
import Editor.Commands.Handlers.SaveLoad
import Editor.Commands.Handlers.Train
import Editor.Commands.Handlers.Sentence

commands :: CmdDescrs
commands = [
    CmdDescr ["help"]                     []                            (CRDStringList "topic")  (cmdShowHelp commands) "Help.",
    CmdDescr ["parse","conllu"]           []                            (CRDFilePathList "path") cmdParseCoNLLU         "Parse CoNNLU files or directories.",
    CmdDescr ["save","sentences"]         [CADFilePath "path.gz"]       CRDNothing               cmdSaveSentences       "Save parsed/loaded sentences.",
    CmdDescr ["load","sentences"]         [CADFilePath "path.gz"]       CRDNothing               cmdLoadSentences       "Load sentences.",
    CmdDescr ["save","encoder"]           [CADFilePath "path.gz"]       CRDNothing               cmdSaveEncoder         "Save word/tag encoder.",
    CmdDescr ["load","encoder"]           [CADFilePath "path.gz"]       CRDNothing               cmdLoadEncoder         "Load word/tag encoder.",
    CmdDescr ["train","tagger"]           [CADFloat "smoothing factor"] CRDNothing               cmdTrainTagger         "Train tagger on sentences loaded.",
    CmdDescr ["save","tagger"]            [CADFilePath "path.gz"]       CRDNothing               cmdSaveTagger          "Save tagger.",
    CmdDescr ["load","tagger"]            [CADFilePath "path.gz"]       CRDNothing               cmdLoadTagger          "Load tagger.",
    CmdDescr ["save","tree","builder"]    [CADFilePath "path.gz"]       CRDNothing               cmdSaveTreeBuilder     "Save dependency tree builder.",
    CmdDescr ["load","tree","builder"]    [CADFilePath "path.gz"]       CRDNothing               cmdLoadTreeBuilder     "Load dependency tree builder.",
    CmdDescr ["tag","sentence"]           []                            (CRDStringList "word")   cmdTagSentence         "Tag sentence.",
    CmdDescr ["describe","tag"]           []                            (CRDIntList "tag")       cmdDescribeTags        "Describe tags.",
    CmdDescr ["quit"]                     []                            CRDNothing               cmdQuit                "Quit program."
    ]

runMainCommand input state = runInput state (words input) commands

cmdQuit :: CommandHandler
cmdQuit = undefined
