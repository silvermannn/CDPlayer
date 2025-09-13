{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands where

import Editor.Commands.Types
import Editor.Commands.Handlers
import Editor.Commands.Handlers.Help
import Editor.Commands.Handlers.CDDB
import Editor.Commands.Handlers.Rules
import Editor.Commands.Handlers.Templates
import Editor.Commands.Handlers.Runner
import Editor.Commands.Handlers.CoNLLU
import Editor.Commands.Handlers.SaveLoad
import Editor.Commands.Handlers.Train
import Editor.Commands.Handlers.Sentence
import Editor.Commands.Handlers.DependencyTree

commands :: CmdDescrs
commands = [
    CmdDescr ["help"]                          []                            (CRDEStringList "topic") (cmdShowHelp commands)     "Help.",
    CmdDescr ["new","cddb"]                    []                            CRDNothing               cmdNewCDDB                 "Create new CD database.",
    CmdDescr ["dump","cddb"]                   []                            CRDNothing               cmdDumpCDDB                "Dump CD database.",
    CmdDescr ["save","cddb"]                   [CADFilePath "path.json"]     CRDNothing               cmdSaveCDDB                "Save CD database.",
    CmdDescr ["load","cddb"]                   [CADFilePath "path.json"]     CRDNothing               cmdLoadCDDB                "Load CD database.",
    CmdDescr ["set","cddb","name"]             [CADString "name"]            CRDNothing               cmdSetCDDBName             "Set name of CD database.",
    CmdDescr ["set","cddb","version"]          [CADInt "version"]            CRDNothing               cmdSetCDDBVersion          "Set version of CD database.",
    CmdDescr ["set","cddb","comment"]          [CADString "comment"]         CRDNothing               cmdSetCDDBComment          "Set comment to CD database.",
    CmdDescr ["add","rule"]                    []                            CRDNothing               cmdAddRule                 "Add rule to current rules.",
    CmdDescr ["renew","rule","ids"]            []                            (CRDEIntList "num")      cmdRenewRules              "Renew ids of specified or all rules from current rules.",
    CmdDescr ["find","rules"]                  []                            (CRDUUIDList "UUID")     cmdFindRules               "Find rules by ids and set them current.",
    CmdDescr ["filter","rules"]                []                            (CRDTree "tree")         cmdFilterRules             "Find rules by syntactic tree and set them current.",
    CmdDescr ["write","rules"]                 []                            (CRDEIntList "num")      cmdWriteRules              "Write specified or all current rules to CD database.",
    CmdDescr ["delete","rules"]                []                            (CRDEIntList "num")      cmdDeleteRules             "Delete specified or all rules from current rules.",
    CmdDescr ["wipe","rules"]                  []                            (CRDEIntList "num")      cmdWipeRules               "Delete specified or all rules from CD databaase.",
    CmdDescr ["set","rules","stop"]            [CADBool "flag"]              (CRDEIntList "num")      cmdSetRulesStop            "Set stop flag for specified or all rules from current rules.",
    CmdDescr ["set","rules","delete","nodes"]  []                            (CRDEIntList "num")      cmdSetRulesDelNodes        "Set list of nodes to delete for current rules.",
    CmdDescr ["add","rule","fact"]             [CADInt "rule"]               (CRDString "fact")        (cmdAddRuleItem updateFacts)      "Add fact <n> in rule.",
    CmdDescr ["update","rule","fact"]          [CADInt "rule"]               (CRDString "fact")        (cmdUpdRuleItem updateFacts)      "Add fact <n> in rule.",
    CmdDescr ["delete","rule","fact"]          [CADInt "rule"]               (CRDEIntList "fact")      (cmdDelRuleItem updateFacts)      "Delete fact <n> in rule.",
    CmdDescr ["add","rule","local"]            [CADInt "rule"]               (CRDString "local")       (cmdAddRuleItem updateLocals)     "Add local variable <n> in rule.",
    CmdDescr ["update","rule","local"]         [CADInt "rule"]               (CRDString "local")       (cmdUpdRuleItem updateLocals)     "Update local variable <n> in rule.",
    CmdDescr ["delete","rule","local"]         [CADInt "rule"]               (CRDEIntList "local")     (cmdDelRuleItem updateLocals)     "Delete local variable <n> in rule.",
    CmdDescr ["add","rule","condition"]        [CADInt "rule"]               (CRDString "condition")   (cmdAddRuleItem updateConditions) "Add condition <n> in rule.",
    CmdDescr ["update","rule","condition"]     [CADInt "rule"]               (CRDString "condition")   (cmdUpdRuleItem updateConditions) "Update condition <n> in rule.",
    CmdDescr ["delete","rule","condition"]     [CADInt "rule"]               (CRDEIntList "condition") (cmdDelRuleItem updateConditions) "Delete condition <n> in rule.",
    CmdDescr ["show","current","rules"]        []                            CRDNothing               cmdShowCurrentRules        "Show current rules.",
    CmdDescr ["add","template"]                [CADString "name"]            (CRDStringList "field")  cmdAddTemplate             "Add template to CD database.",
    CmdDescr ["show","template"]               [CADString "name"]            CRDNothing               cmdShowTemplate            "Show template from CD database.",
    CmdDescr ["delete","template"]             [CADString "name"]            CRDNothing               cmdDeleteTemplate          "Delete template from CD database.",
    CmdDescr ["run","tree"]                    []                            (CRDString "tree")       cmdRunTree                     "Run tree on CD database.",
    CmdDescr ["run","current","tree"]          []                            CRDNothing               cmdRunCurrentTree          "Run current tree on CD database.",
    CmdDescr ["parse","conllu"]                []                            (CRDFilePathList "path") cmdParseCoNLLU             "Parse CoNNLU files or directories.",
    CmdDescr ["save","sentences"]              [CADFilePath "path.gz"]       CRDNothing               cmdSaveSentences           "Save parsed/loaded sentences.",
    CmdDescr ["load","sentences"]              [CADFilePath "path.gz"]       CRDNothing               cmdLoadSentences           "Load sentences.",
    CmdDescr ["save","encoder"]                [CADFilePath "path.gz"]       CRDNothing               cmdSaveEncoder             "Save word/tag encoder.",
    CmdDescr ["load","encoder"]                [CADFilePath "path.gz"]       CRDNothing               cmdLoadEncoder             "Load word/tag encoder.",
    CmdDescr ["train","tagger"]                [CADFloat "smoothing factor"] CRDNothing               cmdTrainTagger             "Train tagger on sentences loaded.",
    CmdDescr ["tag","sentence"]                []                            (CRDStringList "word")   cmdTagSentence             "Tag sentence.",
    CmdDescr ["describe","tag"]                []                            (CRDIntList "tag")       cmdDescribeTags            "Describe tags.",
    CmdDescr ["describe","current","sentence"] []                            CRDNothing               cmdDescribeCurrentSentence "Describe tags of current sentence.",
    CmdDescr ["show","current","sentence"]     []                            CRDNothing               cmdShowCurrentSentence     "Show tags and descriptions of current sentence.",
    CmdDescr ["save","tagger"]                 [CADFilePath "path.gz"]       CRDNothing               cmdSaveTagger              "Save tagger.",
    CmdDescr ["load","tagger"]                 [CADFilePath "path.gz"]       CRDNothing               cmdLoadTagger              "Load tagger.",
    CmdDescr ["save","tree","builder"]         [CADFilePath "path.gz"]       CRDNothing               cmdSaveTreeBuilder         "Save dependency tree builder.",
    CmdDescr ["load","tree","builder"]         [CADFilePath "path.gz"]       CRDNothing               cmdLoadTreeBuilder         "Load dependency tree builder.",
    CmdDescr ["builde","tree"]                 []                            (CRDIntList "tags")      cmdBuildTree               "Build dependency tree.",
    CmdDescr ["describe","relation"]           []                            (CRDIntList "rels")      cmdDescribeRel             "Describe dependency relation.",
    CmdDescr ["describe","current","tree"]     []                            CRDNothing               cmdDescribeCurrentTree     "Describe current dependency tree.",
    CmdDescr ["set","current","tree"]          []                            (CRDTree "tree")         cmdSetCurrentTree          "Set current syntactic tree.",
    CmdDescr ["show","current","tree"]         []                            CRDNothing               cmdShowCurrentTree         "Show current dependency tree",
    CmdDescr ["quit"]                          []                            CRDNothing               cmdQuit                    "Quit program."
    ]

runMainCommand input state = runInput state (words input) commands

cmdQuit :: CommandHandler
cmdQuit = undefined
