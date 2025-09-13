{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Editor.Commands.Handlers where

import Data.List
import Text.Read (readEither)

import Editor.Commands.Types

filterCommandDescr :: [CmdDescr] -> [String] -> [CmdDescr]
filterCommandDescr cds ss = filter (and . zipWith (flip isPrefixOf) ss . keywords) cds

runInput :: ProgramState -> [String] -> CmdDescrs -> IO (Either String ProgramState)
runInput state s cds = case filterCommandDescr cds s of
    [cd] -> callArgs state s cd
    []   -> return $ Left "Command not found"
    _    -> return $ Left "Many commmands fit input"
    where
        callArgs :: ProgramState -> [String] -> CmdDescr -> IO (Either String ProgramState)
        callArgs state ss cd = case applyArgs of
            Left err -> return $ Left err
            Right (as', rs') -> (handler cd) state as' rs'
            where
                applyArgs = do
                    as <- strings2CmdArgs args (mainArguments cd)
                    rs <- strings2CmdRest rest (restArguments cd)
                    return (as, rs)

                (args, rest) = splitAt (length $ mainArguments cd) $ drop (length $ keywords cd) ss

                string2CmdArg s (CADString _) = Right $ CAString s
                string2CmdArg s (CADInt _)    = readEither s >>= \i -> Right $ CAInt i
                string2CmdArg s (CADFloat _)  = readEither s >>= \f -> Right $ CAFloat f

                strings2CmdArgs ss cads = if length ss == length cads
                    then sequence $ zipWith string2CmdArg ss cads
                    else Left "Not enough arguments"

                strings2CmdRest ss (CRDStringList _) = Right $ CRStringList ss
                strings2CmdRest ss (CRDString _)     = Right $ CRString $ unwords ss
                strings2CmdRest ss (CRDIntList _)    = mapM readEither ss >>= \is -> Right $ CRIntList is
                strings2CmdRest [] CRDNothing        = Right CRNothing
                strings2CmdRest _  CRDNothing        = Left "Too many arguments"
