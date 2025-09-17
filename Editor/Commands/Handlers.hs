{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands.Handlers where

import Data.List
import Text.Read (readEither)
import Data.Maybe (mapMaybe)
import Data.UUID (fromString)

import Control.Monad (zipWithM)

import Editor.State
import Editor.Commands.Types

filterCommandDescr :: [CmdDescr] -> [String] -> [CmdDescr]
filterCommandDescr cds ss = filter (and . zipWith isPrefixOf ss . keywords) cds

runInput :: ProgramState -> [String] -> CmdDescrs -> IO (Either String ProgramState)
runInput state argss cds = case filterCommandDescr cds argss of
    [cd] -> callArgs state argss cd
    []   -> return $ Left "Command not found"
    _    -> return $ Left "Many commmands fit input"
    where
        callArgs :: ProgramState -> [String] -> CmdDescr -> IO (Either String ProgramState)
        callArgs pa strings cd = case applyArgs of
            Left err -> return $ Left err
            Right (as', rs') -> handler cd pa as' rs'
            where
                applyArgs = do
                    as <- strings2CmdArgs args (mainArguments cd)
                    rs <- strings2CmdRest rest (restArguments cd)
                    return (as, rs)

                (args, rest) = splitAt (length $ mainArguments cd) $ drop (length $ keywords cd) strings

                string2CmdArg s (CADString _)   = Right $ CAString s
                string2CmdArg s (CADFilePath _) = Right $ CAString s
                string2CmdArg s (CADInt _)      = readEither s >>= \i -> Right $ CAInt i
                string2CmdArg s (CADBool _)     = readEither s >>= \b -> Right $ CABool b
                string2CmdArg s (CADFloat _)    = readEither s >>= \f -> Right $ CAFloat f

                strings2CmdArgs ss cads = if length ss == length cads
                    then zipWithM string2CmdArg ss cads
                    else Left "Not enough arguments"

                strings2CmdRest ss       (CRDEStringList _)  = Right $ CRStringList ss
                strings2CmdRest ss@(_:_) (CRDStringList _)   = Right $ CRStringList ss
                strings2CmdRest ss@(_:_) (CRDFilePathList _) = Right $ CRStringList ss
                strings2CmdRest ss@(_:_) (CRDString _)       = Right $ CRString $ unwords ss
                strings2CmdRest ss       (CRDEIntList _)     = mapM readEither ss >>= \is -> Right $ CRIntList is
                strings2CmdRest ss@(_:_) (CRDIntList _)      = mapM readEither ss >>= \is -> Right $ CRIntList is
                strings2CmdRest ss@(_:_) (CRDUUIDList _)     = Right $ CRUUIDList $ mapMaybe fromString ss
                strings2CmdRest []       CRDNothing          = Right CRNothing
                strings2CmdRest ss@(_:_) (CRDTree _)         = Right $ CRTree $ unwords ss
                strings2CmdRest _        CRDNothing          = Left "Too many arguments"
                strings2CmdRest _        _                   = Left "Arguments mismatch"
