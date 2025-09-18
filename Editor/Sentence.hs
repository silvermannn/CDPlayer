module Editor.Sentence where

import Data.Maybe (catMaybes, fromMaybe)
import Data.List.Extra (chunksOf, intercalate)
import Data.List ((!?))
import Data.Char (toLower)
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
        edges :: Maybe [Int],
        deptree :: Maybe (DependencyTree Int)
    }
    deriving Show

newSentence :: String -> CurrentSentence
newSentence ss = CurrentSentence {
        original = ss,
        tokenized = words $ map toLower ss,
        wordIDs = Nothing,
        ctagged = Nothing,
        tagged = Nothing,
        edges = Nothing,
        deptree = Nothing
    }

showSentence :: (Int, CurrentSentence) -> IO ()
showSentence (n, cs) = do
    putStrLn $ "#" ++ show n
    putStrLn $ "Source sentence:\t\"" ++ original cs ++ "\""
    putStrLn $ "Tokenized sentence:\t[" ++ intercalate " | " (tokenized cs) ++ "]"
    putStrLn $ "Word IDs:\t\t[" ++ intercalate "," (maybe [] (map show) (wordIDs cs)) ++ "]"
    ws <- mapM index2word $ fromMaybe [] $ wordIDs cs
    putStrLn $ "Sentence from IDs:\t\"" ++ unwords (map (fromMaybe unknownWord) ws) ++ "\""
    putStrLn $ "Compound tags:\t\t[" ++ intercalate "," (maybe [] (map show) (ctagged cs)) ++ "]"
    --putStrLn $ "Tag IDs:\t\t[" ++ intercalate "," (maybe [] (map show) (tagged cs)) ++ "]"
    tags <- describeTags $ fromMaybe [] (tagged cs)
    putStrLn $ "Tags:\n" ++ intercalate "\n" (zipWith showWordAndTag (map (fromMaybe unknownWord) ws) (tags)) ++ "\n  |_______\n"
    case deptree cs of
        Nothing -> putStrLn "No dependency tree built yet."
        Just dt -> do
            --putStrLn "Dependency tree IDs:"
            --putStrLn $ show $ deptree cs
            --putStrLn $ drawDTTree show 0 dt
            putStrLn "Dependency tree:"
            dtd <- describeDependencyTree dt
            putStrLn $ drawDTTree id "<ROOT>" dtd
    where
        showWordAndTag w t = "  |  " ++ w ++ replicate (20 - length w) ' ' ++ show t

unknownWord :: String
unknownWord = "<unknown>"

unknownTag :: Tag String
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
    tags' <- mapM describeTag tags
    return $ map (fromMaybe unknownTag) tags'

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

toTree :: Show a => (a -> String) -> a -> DependencyTree a -> Tree String
toTree f l (DTNode a t dts) = Node (show l ++ ": " ++ f  a ++ " " ++ show t) $ map (uncurry (toTree f)) dts

drawDTTree ::  Show a => (a -> String) -> a -> DependencyTree a -> String
drawDTTree f a dt  = drawTree $ toTree f a dt

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
describeCompoundTag ctag = do
    t <- getCompoundPOSTag ctag
    case t of
        Just t' -> return $ Just $ parseTags t'
        Nothing -> return Nothing
    where
        parseTags (pos:features) = Tag pos $ map (\[n, v] -> (n, v)) $ chunksOf 2 features
        parseTags _ = undefined

buidTree :: CurrentSentence -> IO CurrentSentence
buidTree cs = do
    edges <- buildDependencyTree (fromMaybe [] $ ctagged cs)
    return $ cs {edges = edges, deptree = Just $ tree (fromMaybe [] edges) (fromMaybe [] (tagged cs)) (fromMaybe [] $ wordIDs cs)}
    where
        groupEdges xs =  map (\[s, d, l] -> (s, d, l)) (chunksOf 3 xs)
        tree xs ts ws = fromLabeledEdges (groupEdges xs) ts ws 0
        fromLabeledEdges edges tags wrds root = DTNode (extractEl root 0 wrds) (extractEl root (Tag 0[]) tags) ns
            where
                fromRoot = filter (\(a, _, _) -> a == root) edges
                ns = zip (map (\(_, _, b) -> b) fromRoot) $ map (fromLabeledEdges edges tags wrds. (\(_, a, _) -> a)) fromRoot
        extractEl n def xs = fromMaybe def (xs !? (n - 1))