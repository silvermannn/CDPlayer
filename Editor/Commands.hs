{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands where

import Editor.Commands.Types
import Editor.Commands.Handlers
import Editor.Commands.Handlers.Help

commands :: CmdDescrs
commands = [
    CmdDescr ["help"] [] (CRDStringList "topic") (showHelp commands) "Help.",
    CmdDescr ["quit"] [] CRDNothing cmdQuit "Quit program."
    ]

runMainCommand input state = runInput state (words input) commands

cmdQuit :: CommandHandler
cmdQuit = undefined
