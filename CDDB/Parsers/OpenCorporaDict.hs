{-# LANGUAGE OverloadedStrings #-}

module CDDB.Parsers.OpenCorporaDict where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Map as M

import Data.Either (isRight)

import Control.Monad

import Debug.Trace

import CDDB.Dictionary.Dictionary
import CDDB.Dictionary.BidirectionalMap
import CDDB.Dictionary.UniversalDependencies

-- https://opencorpora.org/

parseText path = do
    fileContent <- TIO.readFile path
    return $ parseWords (T.lines fileContent)

parseWords ls = d
    where
        d = newDictionary m
        m = foldr parseWord M.empty ls

parseWord "" m = m
parseWord l m | isRight (TR.decimal l) = m
parseWord l m = M.insertWith (++) w (parserTags rest) m
    where
        (w: rest) = T.splitOn "\t" l

parserTags ts = ts
    where
        (sPOSTag: sfs) = concatMap (T.split isSplit) ts
        isSplit c = c == ' ' || c == ','
        posTag = M.lookup sPOSTag mapPOS >>= findItem uPOSTags
        features = map (flip M.lookup mapFeatures) sfs
        additionalFeatures = M.lookup sPOSTag mapPOSAddition

mapPOS :: M.Map T.Text T.Text
mapPOS = M.fromList [
    ("ADJS", "ADJ"),
    ("ADJF", "ADJ"),
    ("ADVB", "ADV"),
    ("COMP", "ADJ"),
    ("CONJ", "SCONJ"),
    ("GRND", "VERB"),
    ("INFN", "VERB"),
    ("INTJ", "ADJ"),
    ("NOUN", "NOUN"),
    ("NPRO", "PRON"),
    ("NUMR", "NUM"),
    ("PRCL", "PART"),
    ("PRED", "ADV"),
    ("PREP", "ADP"),
    ("PRTF", "VERB"),
    ("PRTS", "VERB"),
    ("PNCT", "PUNCT"),
    ("SYM",  "SYM"),
    ("VERB", "VERB")
    ]

mapPOSAddition = M.fromList [
    ("ADJS", [("variant", "short")]),
    ("GRND", [("verbform","ger")]),
    ("INFN", [("verbform","inf")]),
    ("PRTF", [("verbform","part")]),
    ("PRTS", [("verbform","part"),("variant", "short")])
    ]

mapFeatures = M.fromList [
    ("Ques", ("prontype", "int")),
    ("Dmns", ("prontype", "dem")),

    ("anim", ("animacy", "anim")),
    ("inan", ("animacy", "inan")),

    ("sing", ("number", "sing")),
    ("plur", ("number", "plur")),
    ("Sgtm", ("number", "sing")),
    ("Pltm", ("number", "plur")),

    ("Anum", ("numtype", "ord")),
    ("Coll", ("numtype", "sets")),

    ("masc", ("gender", "masc")),
    ("femn", ("gender", "fem")),
    ("neut", ("gender", "neut")),

    ("impf", ("aspect", "imp")),
    ("perf", ("aspect", "perf")),

    ("pres", ("tense", "pres")),
    ("past", ("tense", "past")),
    ("futr", ("tense", "fut")),

    ("actv", ("voice", "act")),
    ("pssv", ("voice", "pass")),

    ("impr", ("mood", "imp")),
    ("indc", ("mood", "ind")),

    ("Impe", ("subcat", "indir")),
    ("Impx", ("subcat", "indir")),
    ("intr", ("subcat", "intr")),
    ("tran", ("subcat", "tran")),

    ("excl", ("clusivity", "ex")),
    ("incl", ("clusivity", "in")),

    ("nomn", ("case", "nom")),
    ("gent", ("case", "gen")),
    ("gen1", ("case", "gen")),
    ("gen2", ("case", "gen")),
    ("datv", ("case", "dat")),
    ("accs", ("case", "acc")),
    ("acc2", ("case", "acc")),
    ("ablt", ("case", "abl")),
    ("loct", ("case", "loc")),
    ("loc1", ("case", "loc")),
    ("loc2", ("case", "loc")),
    ("voct", ("case", "voc")),

    ("Fixd", ("decl", "zero")),

    ("1per", ("person", "1")),
    ("2per", ("person", "2")),
    ("3per", ("person", "3")),

    ("Abbr", ("abbr", "yes")),
    ("Init", ("abbr", "yes")),
    ("Name", ("nametype", "giv")),
    ("Surn", ("nametype", "sur")),
    ("Patr", ("nametype", "pat")),
    ("Orgn", ("nametype", "com")),
    ("Trad", ("nametype", "com")),
    ("Geox", ("nametype", "geo")),

    ("Supr", ("degree", "sup")),
    ("Cmp",  ("degree", "cmp")),
    ("Cmp2", ("degree", "cmp")),
    ("Poss", ("poss", "yes")),

    ("Erro", ("typo", "yes")),
    ("Dist", ("style", "vrnc")),
    ("Slng", ("style", "slng")),
    ("Arch", ("style", "arch")),
    ("Infr", ("style", "vrnc")),
    ("Litr", ("style", "form"))
    ]