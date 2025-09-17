{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Tree.WordProperty where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)

import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.JSON

type WordProperties = [WordProperty]

data WordProperty = WordProperty Name Name deriving (Eq, Generic)

instance ToJSON WordProperty where
    toJSON t = toJSON $ show t

instance FromJSON WordProperty where
   parseJSON = tryParseJSON

instance Show WordProperty where
    show :: WordProperty -> String
    show (WordProperty name val) = name ++ ": " ++ val

instance Read WordProperty where
    readPrec = do
            L.Ident name <- lexP
            expectP (L.Symbol ":")
            L.Ident val <- lexP
            return $ WordProperty name val
