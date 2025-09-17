module Editor.Sentence where

import Data.Maybe (catMaybes, fromMaybe)
import Data.List.Extra (chunksOf, intercalate)
import Data.Tree

import CDDB.Syntax.Tag
import CDDB.Syntax.DependencyTree

import Support.Support

data CurrentSentence = CurrentSentence {
        original :: String,
        tokenized :: [String],
        wordIDs :: Maybe [Int],
        ctagged :: Maybe [Int],
        tagged :: Maybe (Tags Int),
        deptree :: Maybe (DependencyTree Int)
    }
    deriving Show

newSentence ss = CurrentSentence {
        original = ss,
        tokenized = words ss,
        wordIDs = Nothing,
        ctagged = Nothing,
        tagged = Nothing,
        deptree = Nothing
    }

showSentence :: CurrentSentence -> IO ()
showSentence cs = do
    putStrLn $ "Source sentence: \"" ++ original cs ++ "\""
    putStrLn $ "Tokenized sentence: [" ++ intercalate " | " (tokenized cs) ++ "]"
    putStrLn $ "Word IDs: [" ++ intercalate "," (maybe [] (map show) (wordIDs cs)) ++ "]"
    ws <- mapM index2word $ fromMaybe [] $ wordIDs cs
    putStrLn $ "Tokenized sentence from IDs: \"" ++ unwords (map (fromMaybe unknownWord) ws) ++ "\""
    putStrLn $ "Compound tags: [" ++ intercalate "," (maybe [] (map show) (ctagged cs)) ++ "]"
    putStrLn $ "Tag IDs: [" ++ intercalate "," (maybe [] (map show) (tagged cs)) ++ "]"
    tags <- describeTags $ fromMaybe [] (tagged cs)
    putStrLn $ "Tags: [" ++ intercalate "," (map show tags) ++ "]"
    case deptree cs of
        Nothing -> putStrLn "No dependency tree built yet."
        Just dt -> do
            putStrLn "Dependency tree IDs:"
            putStrLn $ drawDTTree 0 dt
            putStrLn "Dependency tree:"
            dtd <- describeDependencyTree dt
            putStrLn $ drawDTTree "root" dtd

unknownWord = "<unknown>"

unknownTag = Tag "<x>" []

describeDependencyTree :: DependencyTree Int -> IO (DependencyTree String)
describeDependencyTree (DTNode widx tix children) = do
    w <- index2word widx
    t <- describeTag tix
    ch <- mapM (describeDependencyTree . snd) children
    rs <- mapM (describeDepRelTag . fst) children
    return $ DTNode (fromMaybe unknownWord w) (fromMaybe unknownTag t) $ zip rs ch

describeDepRelTag :: Int -> IO String
describeDepRelTag t = do
    Just [rel, modifier] <- getCompoundDeprelTag t
    r <- index2dependencyRelation rel
    m <- index2dependencyRelationModifier modifier
    return $ fromMaybe unknownWord r ++ ":" ++ fromMaybe unknownWord m

describeTags :: Tags Int -> IO (Tags String)
describeTags tags = do
    tags <- mapM describeTag tags
    return $ map (fromMaybe unknownTag) tags

describeTag :: Tag Int -> IO (Maybe (Tag String))
describeTag (Tag pos fs) = do
    posName <- index2POSTag pos
    fsNames <- mapM describeFeature fs
    case posName of
        Just pn-> return $ Just $ Tag pn $ catMaybes fsNames
        _ -> return Nothing

describeFeature :: (Int, Int) -> IO (Maybe (String, String))
describeFeature (n, v) = do
    fn <- index2FeatureName n
    fv <- index2FeatureValue v
    case (fn, fv) of
        (Just fn', Just fv') -> return $ Just (fn', fv')
        _ -> return Nothing

toTree :: Show a => a -> DependencyTree a -> Tree String
toTree l (DTNode a t dts) = Node (show  l ++ ": " ++ show  a ++ " " ++ show t) $ map (uncurry toTree) dts

drawDTTree ::  Show a =>  a -> DependencyTree a -> String
drawDTTree a dt  = drawTree $ toTree a dt

tagSentence :: CurrentSentence -> IO CurrentSentence
tagSentence cs = do
    ws <- mapM word2index (tokenized cs)
    ctags <- tag $ catMaybes ws
    tags <- mapM getCompoundPOSTag $ fromMaybe [] ctags
    return $ cs {wordIDs = sequence ws, ctagged = ctags, tagged = Just $ map (parseTags . fromMaybe []) tags}
    where
        parseTags [] = Tag 0 []
        parseTags (pos:features) = Tag pos $ map (\[n, v] -> (n, v)) $ chunksOf 2 features

describeCompoundTag :: Int -> IO (Maybe (Tag Int))
describeCompoundTag tag = do
    t <- getCompoundPOSTag tag
    case t of
        Just t' -> return $ Just $ parseTags t'
    where
        parseTags (pos:features) = Tag pos $ map (\[n, v] -> (n, v)) $ chunksOf 2 features

buidTree :: CurrentSentence -> IO CurrentSentence
buidTree cs = do
    edges <- buildDependencyTree (fromMaybe [] $ ctagged cs)
    return $ cs {deptree = Just $ tree (fromMaybe [] edges) (fromMaybe [] $ tagged cs) (fromMaybe [] $ wordIDs cs)}
    where
        groupEdges xs =  zipWith (curry (\([s, d, l], (t, w)) -> (s, d, l, t, w))) (chunksOf 3 xs)
        tree xs ts ws = fromLabeledEdges (groupEdges xs $ zip ts ws) (0, Tag 0 [], 0)
        fromLabeledEdges :: [(Int, Int, Int, Tag Int, Int)] -> (Int, Tag Int, Int) -> DependencyTree Int
        fromLabeledEdges edges (root, t, w) = DTNode w t ns
            where
                fromRoot = filter (\(a, _, _, _, _) -> a == root) edges
                ns = zip (map (\(_, _, b, _, _) -> b) fromRoot) $ map (fromLabeledEdges edges. (\(_, a, _, t, w) -> (a, t, w))) fromRoot
