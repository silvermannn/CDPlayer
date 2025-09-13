{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (catch, SomeException)

import CDDB.CDDB

import Editor.Settings
import Editor.Commands
import Editor.Commands.Types

import Support.Support

initialProgramState :: Editor.Settings.Settings -> IO ProgramState
initialProgramState settings = do
    se <- initEngine
    return $ ProgramState
        {
            settings = settings,
            cddb = emptyCDDB,
            currentRules = [],
            isNotSaved = True,
            currentTemplate = Nothing,
            supportEngine = se,
            taggedSentence = Nothing
        }

-- TODO: Use agreedNotToSave
agreedNotToSave :: InputT IO Bool
agreedNotToSave = do
    answer <- getInputChar "CDDB is not saved. Dou you really want to quit (y/N)?"
    return $ answer == Just 'N' || answer == Just 'n'

main :: IO ()
main = do
        startSettings <- readSettings
        state <- initialProgramState startSettings
        runInputTWithPrefs (haskelinePrefsFromSettings startSettings) (haskelineSettionsFromSettings startSettings) $ loop state
        clearEngine $ supportEngine state
    where
        loop state = do
            minput <- getInputLine "CDDB> "
            case minput of
                Nothing -> return ()
                Just "quit" -> do
                        liftIO $ writeSettings $ settings state
                        return ()
                Just input -> flip catch exceptonHandler $ do
                    res <- liftIO $ runMainCommand input state
                    case res of
                        Left errorMessage -> outputStrLn errorMessage >> loop state
                        Right state' -> loop state'

        exceptonHandler :: SomeException -> InputT IO ()
        exceptonHandler ex = outputStrLn $ "Exception" ++ show ex
