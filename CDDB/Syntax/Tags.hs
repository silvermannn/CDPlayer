module CDDB.Syntax.Tags where

-- https://opencorpora.org/dict.php?act=gram

data POS = 
      ADJS
    | ADJF
    | ADVB
    | COMP
    | CONJ
    | GRND
    | INFN
    | INTJ
    | NOUN
    | NPRO
    | NUMR
    | PRCL
    | PRED
    | PREP
    | PRTF
    | PRTS
    | PNCT
    | SYM
    | VERB
    deriving (Eq, Ord, Bounded, Show)

data Feature = 
      Anim
    | Inan
    | Masc
    | Femn
    | Ms_f
    | Neut
    | Sing
    | Plur
    | Sgtm
    | Pltm
    | Fixd
    | Nomn
    | Gent
    | Datv
    | Accs
    | Ablt
    | Loct
    | Voct
    | Gen1
    | Gen2
    | Acc2
    | Loc1
    | Loc2
    | Abbr
    | Name
    | Surn
    | Patr
    | Geox
    | Orgn
    | Trad
    | Subx
    | Supr
    | Qual
    | Apro
    | Anum
    | Poss
    | Cmp2
    | Perf
    | Impf
    | Tran
    | Intr
    | Impe
    | Impx
    | Mult
    | Refl
    | Per1
    | Per2
    | Per3
    | Pres
    | Past
    | Futr
    | Indc
    | Impr
    | Incl
    | Excl
    | Actv
    | Pssv
    | Infr
    | Slng
    | Arch
    | Litr
    | Erro
    | Dist
    | Ques
    | Dmns
    | Prnt
    | Fimp
    | Prdx
    | Coun
    | Coll
    | Af_p
    | Inmx
    | Vpre
    | Anph
    | Init
    | Adjx
    | V_ey
    | V_oy
    | V_ej
    | V_be
    | V_en
    | V_ie
    | V_bi
    | V_sh
    | Hypo
    deriving (Eq, Ord, Bounded, Show)

data DependencyRelation = DependencyRelation
    deriving Show