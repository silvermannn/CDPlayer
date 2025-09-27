{-# LANGUAGE OverloadedStrings #-}

module CDDB.Parsers.OpenCorporaDict where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Either (isRight)
import Data.Either.Extra (maybeToEither)
import Data.List (uncons)
import Data.Maybe (fromMaybe)

import Control.Monad

import CDDB.Dictionary.Dictionary
import CDDB.Dictionary.BidirectionalMap
import CDDB.Dictionary.UniversalDependencies

import CDDB.Syntax.Tag

-- https://opencorpora.org/

parseText :: [FilePath] -> IO (Either T.Text Dictionary)
parseText paths = do
    fileContent <- mapM TIO.readFile paths
    return $ parseWords (concatMap T.lines fileContent)

parseWords :: [T.Text] -> Either T.Text Dictionary
parseWords ls = do
    m <- foldM parseWord M.empty ls
    return $ newDictionary m

parseWord :: M.Map T.Text (S.Set Tag) -> T.Text -> Either T.Text (M.Map T.Text (S.Set Tag))
parseWord m "" = Right m
parseWord m l | isRight (TR.decimal l) = Right m
parseWord m l = do
    (w, rest) <- maybeToEither ("No \\t in " <> l) $ uncons $ T.splitOn "\t" l
    parsed <- parseTags rest
    return $ M.insertWith S.union (T.toLower w) parsed m

parseTags :: [T.Text] -> Either T.Text (S.Set Tag)
parseTags ts = do
    (sPOSTag, sfs) <- maybeToEither "Cannot split to POS tag and features" $ uncons $ concatMap (filter (not . T.null) . T.split isSplit) ts
    posTag <- maybeToEither ("Tag " <> sPOSTag <> " not found in " <> T.intercalate ", " ts) $ M.lookup sPOSTag mapPOS >>= findItem uPOSTags
    mainFeaturesS <- maybeToEither ("Cannot map features in " <> T.intercalate ", " ts) $ mapM (`M.lookup` mapFeatures) sfs
    featurePairs <- Right $ featuresS (concat mainFeaturesS) sPOSTag
    names <- maybeToEither ("Cannot map feature names in " <> T.intercalate ", " (map fst featurePairs)) $ mapM (findItem featureNames . fst) featurePairs
    values <- maybeToEither ("Cannot map feature values in " <> T.intercalate ", " (map snd featurePairs)) $ mapM (findItem featureValues . snd) featurePairs
    return $ S.singleton $ Tag posTag $ zip names values
    where
        isSplit c = c == ' ' || c == ','
        featuresS m t = m ++ fromMaybe [] (M.lookup t mapPOSAddition)

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

mapPOSAddition :: M.Map T.Text [(T.Text, T.Text)]
mapPOSAddition = M.fromList [
    ("ADJS", [("Variant", "Short")]),
    ("GRND", [("VerbForm","Ger")]),
    ("INFN", [("VerbForm","Inf")]),
    ("PRTF", [("VerbForm","Part")]),
    ("PRTS", [("VerbForm","Part"),("Variant", "Short")])
    ]

mapFeatures :: M.Map T.Text [(T.Text, T.Text)]
mapFeatures = M.fromList [
    ("Ques", [("PronType", "Int")]),
    ("Dmns", [("PronType", "Dem")]),

    ("anim", [("Animacy", "Anim")]),
    ("inan", [("Animacy", "Inan")]),

    ("sing", [("Number", "Sing")]),
    ("plur", [("Number", "Plur")]),
    ("Sgtm", [("Number", "Stan")]),
    ("Pltm", [("Number", "Ptan")]),

    ("Anum", [("NumType", "Ord")]),
    ("Coll", [("NumType", "Sets")]),

    ("masc", [("Gender", "Masc")]),
    ("femn", [("Gender", "Fem")]),
    ("neut", [("Gender", "Neut")]),
    ("ms-f", [("Gender", "Com")]),
    ("Ms-f", [("Gender", "Com")]),
    ("GNdr", [("Gender", "Com")]),

    ("impf", [("Aspect", "Imp")]),
    ("perf", [("Aspect", "Perf")]),

    ("pres", [("Tense", "Pres")]),
    ("past", [("Tense", "Past")]),
    ("futr", [("Tense", "Fut")]),

    ("actv", [("Voice", "Act")] ),
    ("pssv", [("Voice", "Pass")]),

    ("impr", [("Mood", "Imp")]),
    ("indc", [("Mood", "Ind")]),

    ("Impe", [("Subcat", "Indir")]),
    ("Impx", [("Subcat", "Indir")]),
    ("intr", [("Subcat", "Intr")]),
    ("tran", [("Subcat", "Tran")]),

    ("excl", [("Clusivity", "Ex")]),
    ("incl", [("Clusivity", "In")]),

    ("nomn", [("Case", "Nom")]),
    ("gent", [("Case", "Gen")]),
    ("gen1", [("Case", "Gen")]),
    ("gen2", [("Case", "Gen")]),
    ("datv", [("Case", "Dat")]),
    ("accs", [("Case", "Acc")]),
    ("acc2", [("Case", "Acc")]),
    ("ablt", [("Case", "Abl")]),
    ("loct", [("Case", "Loc")]),
    ("loc1", [("Case", "Loc")]),
    ("loc2", [("Case", "Loc")]),
    ("voct", [("Case", "Voc")]),

    ("Fixd", [("Decl", "Zero")]),

    ("1per", [("Person", "1")]),
    ("2per", [("Person", "2")]),
    ("3per", [("Person", "3")]),

    ("Abbr", [("Abbr", "Yes")]),
    ("Init", [("Abbr", "Yes")]),
    ("Name", [("NameType", "Giv")]),
    ("Surn", [("NameType", "Sur")]),
    ("Patr", [("NameType", "Pat")]),
    ("Orgn", [("NameType", "Com")]),
    ("Trad", [("NameType", "Com")]),
    ("Geox", [("NameType", "Geo")]),

    ("Supr", [("Degree", "Sup")]),
    ("Cmp",  [("Degree", "Cmp")]),
    ("Cmp2", [("Degree", "Cmp")]),
    ("Poss", [("Poss", "Yes")]),

    ("Erro", [("Typo", "Yes")]),
    ("Dist", [("Style", "Vrnc")]),
    ("Slng", [("Style", "Slng")]),
    ("Arch", [("Style", "Arch")]),
    ("Infr", [("Style", "Vrnc")]),
    ("Litr", [("Style", "Form")]),

    ("Adjx", []),
    ("Af-p", []),
    ("Anph", []),
    ("Apro", []),
    ("Coun", []),
    ("Fimp", []),
    ("INFN", []),
    ("Inmx", []),
    ("Hypo", []),
    ("Prdx", []),
    ("Prnt", []),
    ("Qual", []),
    ("Subx", []),
    ("V-be", []),
    ("V-en", []),
    ("V-ie", []),
    ("V-bi", []),
    ("V-sh", []),
    ("V-oy", []),
    ("V-ej", []),
    ("V-ey", []),
    ("Vpre", [])
    ]