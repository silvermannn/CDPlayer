module CDDB.Syntax.TreeBuilder where

import CDDB.Syntax.Tag
import CDDB.Syntax.DependencyTree

type Weight = Double

data RuleDirection = SearchLeft | SearchRight deriving (Eq, Ord, Show)

type SentenceItem = (Int, Int, Tag, Bool)
type Sentence = [SentenceItem]

data BuildRule = BuildRule {
        relation :: DependencyRelation,
        direction :: RuleDirection,
        sourceFilter :: Tag,
        destinationFilter :: Tag,
        weight :: Weight
    } deriving Show

newtype BuildRuleSet = BuildRuleSet [BuildRule] deriving Show

matchTag t1 t2 = t1 == t2

matchTagInSs :: Tag -> SentenceItem -> [(Int, Int, Tag)]
matchTagInSs t1 (w, pos, t2, avail) = if avail && t1 == t2 then [(w, pos, t2)] else []

mapSentence match f [] = []
mapSentence match f (s:ss) = [(w, pos, t, f s:ss) | (w, pos, t) <- match s] ++ (map (fmap (s:)) $ mapSentence match f ss)

checkDirection dir posCh posHead = dir == SearchLeft && posCh < posHead || dir == SearchRight && posCh > posHead

mapRule (ww, ss, dt) (BuildRule rel dir srct dstt wt) = [(ww * wt, ss', dt') | (wid1, pos1, t1, ss') <- mapSentence (matchTagInSs dstt) (fmap $ const False) ss, (wid2, pos2, t2, dt') <- findAllAndModifyTrees (matchTag srct) (insertTag rel wid1 pos1 t1) dt, checkDirection dir pos1 pos2]

mapRules rs p = (null app, p: app)
    where
        app = concatMap (mapRule p) rs

checkFinished (_, ts, _) = not $ any snd ts

applyRules rs ps = finished ++ (if null res then ps else applyRules rs res)
    where
        app = map (mapRules rs) ps
        finished = concatMap snd app
        res = concatMap snd $ filter fst app
