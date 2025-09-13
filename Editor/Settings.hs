{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Settings where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Control.Monad.Catch (catch, SomeException)

import CDDB.Tree.Syntax
import CDDB.Runner.Context

data Settings = Settings {
        cddbFileName :: Maybe FilePath,
        cddbTree :: Maybe SyntacticTree,
        historyFile :: String,
        autoAddHistory :: Bool,
        maxRecursionDepth :: RecursionDepth
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

defalultSettings :: Settings
defalultSettings = Settings {
        cddbFileName = Nothing,
        cddbTree = Nothing,
        historyFile = ".cddb_history",
        autoAddHistory = True,
        maxRecursionDepth = 10
    }

readSettings :: IO Settings
readSettings = do
    fileContent <- flip catch exceptonHandler $ B.readFile settingsFilename
    case (decode :: B.ByteString -> Maybe Settings) fileContent of
        Nothing -> return defalultSettings
        Just s -> return s
    where
        exceptonHandler :: SomeException -> IO B.ByteString
        exceptonHandler _ = return B.empty

writeSettings :: Settings -> IO ()
writeSettings settings = do
    B.writeFile settingsFilename $  encode (toJSON settings)

settingsFilename :: String
settingsFilename = ".editor_state.json"
