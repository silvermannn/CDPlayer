module CDDB.Dictionary.UniversalDependencies where

import Data.Bimap
import Data.List.Extra (zipFrom)
import Data.Maybe (fromJust)

makeBimap :: [String] -> Bimap Int String
makeBimap = fromList . zipFrom 1

-- https://universaldependencies.org/

uPOSTags = makeBimap [
    "<>",   -- service tag
    "ADJ",  -- adjective
    "ADP",  -- adposition
    "ADV",  -- adverb
    "AUX",  -- auxiliary
    "CCONJ",-- coordinating conjunction
    "DET",  -- determiner
    "INTJ", -- interjection
    "NOUN", -- noun
    "NUM",  -- numeral
    "PART", -- particle
    "PRON", -- pronoun
    "PROPN",-- proper noun
    "PUNCT",-- punctuation
    "SCONJ",-- subordinating conjunction
    "SYM",  -- symbol
    "VERB", -- verb
    "X"     -- other
    ]

featureNames = makeBimap [
    "PronType","Gender","VerbForm","NumType","Animacy","Mood","Poss","Tense","Reflex",
    "Number","Aspect","Other","Case","Voice","Abbr","Definite","Evident","Typo","Deixis","Polarity",
    "Foreign","DeixisRef","Person","ExtPos","Degree","Polite","Clusivity","NameType","Subcat","Style",
    {-"NounClass",-} -- skipped intentionally
    "Variant",       -- added for short verbs/adjs
    "Decl"           -- added for noun w/o declension
    ]

featureValues = makeBimap [
{-PronType-}  "Art", "Dem", "Emp", "Exc", "Ind", "Int", "Neg", "Prs", "Rcp", "Rel", "Tot",
{-Gender-}    "Com", "Fem", "Masc", "Neut",
{-VerbForm-}  "Conv", "Fin", "Gdv", "Ger", "Inf", "Part", "Sup", "Vnoun",
{-NumType-}   "Card", "Dist", "Frac", "Mult", "Ord", "Range", "Sets",
{-Animacy-}   "Anim", "Hum", "Inan", "Nhum",
{-NounClass -} --  skipped intentionally
{-Mood-}      "Adm", "Cnd", "Des", "Imp", "Ind", "Int", "Irr", "Jus", "Nec", "Opt", "Pot", "Prp", "Qot", "Sub",
{-Poss-}      "Yes",
{-Tense-}     "Fut", "Imp", "Past", "Pqp", "Pres",
{-Reflex-}    "Yes",
{-Number-}    "Coll", "Count", "Dual", "Grpa", "Grpl", "Inv", "Pauc", "Plur", "Ptan", "Sing", "Stan", "Tri",
{-Aspect-}    "Hab", "Imp", "Iter", "Perf", "Prog", "Prosp",
{-Case-}      "Abs", "Acc", "Erg", "Nom",
              "Abe", "Ben", "Cau", "Cmp", "Cns", "Com", "Dat", "Dis", "Equ", "Gen", "Ins", "Par", "Tem", "Tra", "Voc",
              "Abl", "Add", "Ade", "All", "Del", "Ela", "Ess", "Ill", "Ine", "Lat", "Loc", "Per", "Sbe", "Sbl", "Spl", "Sub", "Sup", "Ter",
{-Voice-}     "Act", "Antip", "Bfoc", "Cau", "Dir", "Inv", "Lfoc", "Mid", "Pass", "Rcp",
{-Abbr-}      "Yes",
{-Definite-}  "Com", "Cons", "Def", "Ind", "Spec",
{-Evident-}   "Fh", "Nfh",
{-Typo-}      "Yes",
{-Deixis-}    "Abv", "Bel", "Even", "Med", "Nvis", "Prox", "Remt",
{-Polarity-}  "Neg", "Pos",
{-Foreign-}   "Yes",
{-DeixisRef-} "1", "2",
{-Person-}    "0", "1", "2", "3", "4",
{-ExtPos-}    "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "PRON", "PROPN", "SCONJ",
{-Degree-}    "Abs", "Aug", "Cmp", "Dim", "Equ", "Pos", "Sup",
{-Polite-}    "Elev", "Form", "Humb", "Infm",
{-Clusivity-} "Ex", "In",
{-NameType-}  "Com", "Geo", "Giv", "Nat", "Oth", "Pat", "Pro", "Prs", "Sur", "Zoon",
{-Subcat-}    "Ditr", "Indir", "Intr", "Tran",
{-Style-}     "Arch", "Coll", "Expr", "Form", "Rare", "Slng", "Vrnc", "Vulg",
{-Variant-}   "Short",
{-Decl-}      "Zero"
    ]

dependencyRelations = makeBimap [
{-Core arguments-}      "nsubj", "obj", "iobj", "csubj", "ccomp", "xcomp",
{-Non-core dependents-} "obl", "vocative", "expl", "dislocated", "advcl", "advmod", "discourse", "aux", "cop", "mark",
{-Nominal dependents-}  "nmod", "appos", "nummod", "acl", "amod", "det", "clf", "case",
{-Coordination-}        "conj", "cc",
{-Headless-}            "fixed", "flat",
{-Loose-}               "list", "parataxis",
{-Special-}             "compound", "orphan", "goeswith", "reparandum",
{-Other-}               "punct", "root", "dep"
    ]

dependencyRelationModifiers = makeBimap [
    "relcl", "pass", "outer", "impers", "pv", "agent", "emph", "lmod", "preconj", "lvc",
    "prt", "redup", "svc", "numgov", "nummod", "foreign", "name", "tmod", "poss", "gov", "arg"
    ]

rootDependency :: Int
rootDependency = fromJust $ lookupR "root" dependencyRelations

serviceTag :: Int
serviceTag = fromJust $ lookupR "<>" uPOSTags
