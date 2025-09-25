module CDDB.Syntax.TreeBuilder where

import Data.Tree

import CDDB.Syntax.Tree

data RuleDirection = SearchLeft | SearchRight deriving Show

type Weight = Double

type Sentence = [(Tag, Bool)]

data Rule = Rule {
        relation :: DependencyRelation,
        direction :: RuleDirection,
        sourceFilter :: Tag,
        destinationFilter :: Tag,
        weight :: Weight
    } deriving Show

newtype RuleSet = RuleSet [Rule] deriving Show

matchTag t1 t2 = t1 == t2

matchTagInSs t1 (t2, avail) = avail && t1 == t2

mapSentence match f [] = []
mapSentence match f (s:ss) | match s = [(s, f s:ss)] ++ (map (fmap (s:)) $ mapSentence match f ss)
mapSentence match f (s:ss)           = map (fmap (s:)) $ mapSentence match f ss

mapRule (ww, ss, dt) (Rule r _ srct dstt w) = [(ww * w, ss', dt') | (t, ss') <- mapSentence (matchTagInSs dstt) (fmap $ const False) ss, dt' <- findAllAndModifyTrees (matchTag srct) (insertTag r (fst t)) dt]

mapRules rs p = (null app, p: app)
    where
        app = concatMap (mapRule p) rs

checkFinished (_, ts, _) = not $ or $ map snd ts

applyRules rs ps = finished ++ (if null res then ps else applyRules rs res)
    where
        app = map (mapRules rs) ps
        finished = concatMap snd app
        res = concatMap snd $ filter fst $ app

ts = [(Tag 1 [], True), (Tag 2 [], True)]

r1 = Rule 100 SearchLeft (Tag 0 []) (Tag 1 []) 3.0
r2 = Rule 200 SearchLeft (Tag 1 []) (Tag 2 []) 0.171
rs = [r1, r2]

