module Editor.Commands.Handlers.CDDB where

import Data.Time (UTCTime(..), getCurrentTime)
import Data.Aeson (encode, decode, toJSON)
import qualified Data.ByteString.Lazy as B

import System.IO

import CDDB.CDDB

import Editor.State
import Editor.Commands.Types

cmdSaveCDDB :: CommandHandler
cmdSaveCDDB state [CAString path] CRNothing = do
    today <- getCurrentTime
    let updatedCDDB = (cddb state) {date = today} in do
        B.writeFile path $ encode (toJSON updatedCDDB)
        return $ Right state {cddb = updatedCDDB}

cmdLoadCDDB :: CommandHandler
cmdLoadCDDB state [CAString path] CRNothing = do
    handle <- openFile path ReadMode
    fileContent <- B.hGetContents handle
    case (decode :: B.ByteString -> Maybe CDDB) fileContent of
        Nothing -> return $ Left "Error reading file."
        Just cddb' -> return $ Right state {cddb = cddb'}

cmdNewCDDB :: CommandHandler
cmdNewCDDB state [] CRNothing = do
    return $ Right state {cddb = emptyCDDB}

cmdDumpCDDB :: CommandHandler
cmdDumpCDDB state [] CRNothing = do
    print (cddb state)
    return $ Right state

cmdSetCDDBName :: CommandHandler
cmdSetCDDBName state [CAString n] CRNothing = do
    return $ Right state {cddb = (cddb state) {name = n}}

cmdSetCDDBVersion :: CommandHandler
cmdSetCDDBVersion state [CAInt v] CRNothing = do
    return $ Right state {cddb = (cddb state) {version = toInteger v}}

cmdSetCDDBComment :: CommandHandler
cmdSetCDDBComment state [CAString c] CRNothing = do
    return $ Right state {cddb = (cddb state) {cddbcomment = c}}

