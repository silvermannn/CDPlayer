{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (catch, SomeException)

import Editor.Settings
import Editor.State
import Editor.Commands
import Editor.Commands.Completion

import Support.Support

-- Haskelline stuff
haskelinePrefsFromSettings :: Editor.Settings.Settings -> Prefs
haskelinePrefsFromSettings _ = defaultPrefs

haskelineSettionsFromSettings :: Editor.Settings.Settings ->  System.Console.Haskeline.Settings IO
haskelineSettionsFromSettings settings = System.Console.Haskeline.Settings {
        System.Console.Haskeline.complete = editorComplete,
        System.Console.Haskeline.historyFile = Just $ Editor.Settings.historyFile settings,
        System.Console.Haskeline.autoAddHistory = Editor.Settings.autoAddHistory settings
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
