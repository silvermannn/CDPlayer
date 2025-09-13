{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands where

import Editor.Commands.Types
import Editor.Commands.Handlers
import Editor.Commands.Handlers.Help
import Editor.Commands.Handlers.CDDB
import Editor.Commands.Handlers.Templates
import Editor.Commands.Handlers.CoNLLU
import Editor.Commands.Handlers.SaveLoad
import Editor.Commands.Handlers.Train
import Editor.Commands.Handlers.Sentence
import Editor.Commands.Handlers.DependencyTree

commands :: CmdDescrs
commands = [
    CmdDescr ["help"]                  []                            (CRDEStringList "topic") (cmdShowHelp commands) "Help.",
    CmdDescr ["new","cddb"]            []                            CRDNothing               cmdNewCDDB             "Create new CD database.",
    CmdDescr ["dump","cddb"]           []                            CRDNothing               cmdDumpCDDB            "Dump CD database.",
    CmdDescr ["save","cddb"]           [CADFilePath "path.json"]     CRDNothing               cmdSaveCDDB            "Save CD database.",
    CmdDescr ["load","cddb"]           [CADFilePath "path.json"]     CRDNothing               cmdLoadCDDB            "Load CD database.",
    CmdDescr ["set","cddb","name"]     [CADString "name"]            CRDNothing               cmdSetCDDBName         "Set name of CD database.",
    CmdDescr ["set","cddb","version"]  [CADInt "version"]            CRDNothing               cmdSetCDDBVersion      "Set version of CD database.",
    CmdDescr ["set","cddb","comment"]  [CADString "comment"]         CRDNothing               cmdSetCDDBComment      "Set comment to CD database.",
    CmdDescr ["add","template"]        [CADString "name"]            (CRDStringList "field")  cmdAddTemplate         "Add template to CD database.",
    CmdDescr ["show","template"]       [CADString "name"]            CRDNothing               cmdShowTemplate        "Show template from CD database.",
    CmdDescr ["delete","template"]     [CADString "name"]            CRDNothing               cmdDeleteTemplate      "Delete template from CD database.",
    CmdDescr ["parse","conllu"]        []                            (CRDFilePathList "path") cmdParseCoNLLU         "Parse CoNNLU files or directories.",
    CmdDescr ["save","sentences"]      [CADFilePath "path.gz"]       CRDNothing               cmdSaveSentences       "Save parsed/loaded sentences.",
    CmdDescr ["load","sentences"]      [CADFilePath "path.gz"]       CRDNothing               cmdLoadSentences       "Load sentences.",
    CmdDescr ["save","encoder"]        [CADFilePath "path.gz"]       CRDNothing               cmdSaveEncoder         "Save word/tag encoder.",
    CmdDescr ["load","encoder"]        [CADFilePath "path.gz"]       CRDNothing               cmdLoadEncoder         "Load word/tag encoder.",
    CmdDescr ["train","tagger"]        [CADFloat "smoothing factor"] CRDNothing               cmdTrainTagger         "Train tagger on sentences loaded.",
    CmdDescr ["tag","sentence"]        []                            (CRDStringList "word")   cmdTagSentence         "Tag sentence.",
    CmdDescr ["describe","tag"]        []                            (CRDIntList "tag")       cmdDescribeTags        "Describe tags.",
    CmdDescr ["save","tagger"]         [CADFilePath "path.gz"]       CRDNothing               cmdSaveTagger          "Save tagger.",
    CmdDescr ["load","tagger"]         [CADFilePath "path.gz"]       CRDNothing               cmdLoadTagger          "Load tagger.",
    CmdDescr ["save","tree","builder"] [CADFilePath "path.gz"]       CRDNothing               cmdSaveTreeBuilder     "Save dependency tree builder.",
    CmdDescr ["load","tree","builder"] [CADFilePath "path.gz"]       CRDNothing               cmdLoadTreeBuilder     "Load dependency tree builder.",
    CmdDescr ["builde","tree"]         []                            (CRDIntList "tags")      cmdBuildTree           "Build dependency tree.",
    CmdDescr ["describe","relation"]   []                            (CRDIntList "rels")      cmdDescribeRel         "Describe dependency relation.",
    CmdDescr ["quit"]                  []                            CRDNothing               cmdQuit                "Quit program."
    ]

runMainCommand input state = runInput state (words input) commands

cmdQuit :: CommandHandler
cmdQuit = undefined
