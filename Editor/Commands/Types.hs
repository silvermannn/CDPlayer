{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands.Types where

import CDDB.Types (Name)
import CDDB.CDDB
import CDDB.Rules

import Support.Support

import Editor.Settings

data ProgramState = ProgramState {
        settings :: Settings,
        cddb :: CDDB,
        currentRules :: [(RuleId, Rule)],
        isNotSaved :: Bool,
        currentTemplate :: Maybe Name,
        supportEngine :: Handle,
        taggedSentence :: Maybe [Int]
    }

data CmdArgDescr = CADString String | CADInt String | CADFloat String deriving (Show, Eq)

data CmdRestDescr = CRDStringList String | CRDString String | CRDIntList String | CRDNothing deriving (Show, Eq)

data CmdDescr = CmdDescr {
        keywords :: [String],
        mainArguments :: [CmdArgDescr],
        restArguments :: CmdRestDescr,
        handler :: CommandHandler,
        helpString :: String
    }

type CmdDescrs = [CmdDescr]

data CmdArg = CAString String | CAInt Int | CAFloat Float deriving (Show, Eq)

data CmdRest = CRStringList [String] | CRString String | CRIntList [Int] | CRNothing deriving (Show, Eq)

type CommandHandler = ProgramState -> [CmdArg] -> CmdRest -> IO (Either String ProgramState)
