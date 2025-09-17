module Editor.Support where

import Data.Maybe (catMaybes)
import Data.List.Extra (chunksOf)

import CDDB.Syntax.Tag
import CDDB.Syntax.DependencyTree

import Support.Support

type Result a = IO (Maybe a)

tagSentence :: [String] -> Result [Int]
tagSentence ss = do
    ws <- mapM word2index ss
    tags <- tag $ catMaybes ws
    case tags of
        Nothing -> return Nothing
        Just tags' -> return $ Just tags'

describeCompoundTag :: Int -> Result (Tag Int)
describeCompoundTag tag = do
    t <- getCompoundPOSTag tag
    case t of
        Just t' -> return $ Just $ parseTags t'
    where
        parseTags (pos:features) = Tag pos $ map (\[n, v] -> (n, v)) $ chunksOf 2 features

describeTag :: Tag Int -> Result (Tag String)
describeTag (Tag pos fs) = do
    posName <- index2POSTag pos
    fsNames <- mapM describeFeature fs
    case posName of
        Just pn-> return $ Just $ Tag pn $ catMaybes fsNames
        _ -> return Nothing

describeFeature :: (Int, Int) -> Result (String, String)
describeFeature (n, v) = do
    fn <- index2FeatureName n
    fv <- index2FeatureValue v
    case (fn, fv) of
        (Just fn', Just fv') -> return $ Just (fn', fv')
        _ -> return Nothing

describeTags :: Tags Int -> Result (Tags String)
describeTags tags = do
    tags <- mapM describeTag tags
    return $ Just $ catMaybes tags

buidTree :: [Int] -> Result (DependencyTree Int)
buidTree tags = do
    edges <- buildDependencyTree tags
    ctags <- mapM describeCompoundTag tags
    case edges of
        Just es -> return $ Just $ tree es $ catMaybes ctags
        _ -> return Nothing
    where
        groupEdges xs =  zipWith (curry (\([s, d, l], t) -> (s, d, l, t))) (chunksOf 3 xs)
        tree xs ts = fromLabeledEdges (groupEdges xs ts) (0, Tag 0 [])
        fromLabeledEdges :: Eq a => [(a, a, a, Tag a)] -> (a, Tag a) -> DependencyTree a
        fromLabeledEdges edges (root, t) = DTNode root t ns
            where
                fromRoot = filter (\(a, _, _, _) -> a == root) edges
                ns = zip (map (\(_, _, b, _) -> b) fromRoot) $ map (fromLabeledEdges edges. (\(_, a, _, t) -> (a, t))) fromRoot

describeTree :: DependencyTree Int -> Result (DependencyTree String)
describeTree tree = do
    return Nothing

