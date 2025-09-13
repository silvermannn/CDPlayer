{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands.Types where

import Editor.State

data CmdArgDescr = CADString String | CADFilePath String | CADInt String | CADFloat String deriving (Show, Eq)

data CmdRestDescr = CRDEStringList String | CRDStringList String | CRDFilePathList String | CRDString String | CRDIntList String | CRDNothing deriving (Show, Eq)

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

describeCommand (CmdDescr kw ma ra _ _) = unwords kw ++ " " ++ unwords (map describeCommandDef ma) ++ " " ++ describeCommandRest ra
    where
        describeCommandDef (CADString s) = "<" ++ s ++ ">"
        describeCommandDef (CADFilePath s) = "<path: " ++ s ++ ">"
        describeCommandDef (CADInt s) = "<int: " ++ s ++ ">"
        describeCommandDef (CADFloat s) = "<float: " ++ s ++ ">"

        describeCommandRest (CRDEStringList s) = "{<" ++ s ++ "> ... <" ++ s ++ ">}"
        describeCommandRest (CRDStringList s) = "<" ++ s ++ "> ... <" ++ s ++ ">"
        describeCommandRest (CRDFilePathList s) = "<" ++ s ++ "> ... <" ++ s ++ ">"
        describeCommandRest (CRDString s) = "<" ++ s ++ ">"
        describeCommandRest (CRDIntList s) = "<int:" ++ s ++ "> ... <int:" ++ s ++ ">"
        describeCommandRest CRDNothing = ""
