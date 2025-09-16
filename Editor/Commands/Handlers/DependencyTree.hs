module Editor.Commands.Handlers.DependencyTree where

import Data.Maybe (fromMaybe)
import Data.List.Extra (chunksOf)
import Data.Tree

import CDDB.Syntax.DependencyTree

import Editor.State
import Editor.Commands.Types
import Editor.Support

cmdBuildTree :: CommandHandler
cmdBuildTree state [] (CRIntList tags) = do
    -- edges <- buidTree tags
    -- print edges
    return $ Right state -- {dependencyTree = edges}

cmdDescribeRel :: CommandHandler
cmdDescribeRel state [] (CRIntList depRels) = do
    -- tagDescrs <- mapM describeRel depRels
    -- print tagDescrs
    return $ Right state

cmdDescribeCurrentTree :: CommandHandler
cmdDescribeCurrentTree state [] (CRIntList depRels) = case (currentDTree state) of
    Nothing -> return $ Left ""
    Just depRels -> do
        tagDescrs <- describeTree depRels
        print tagDescrs
        return $ Right state {currentDTreeStr = tagDescrs}

cmdShowCurrentTree :: CommandHandler
cmdShowCurrentTree state [] CRNothing = case (currentDTree state) of
    Nothing -> return $ Left "No current tree exists yet."
    Just tags -> do
        print tags
        print $ currentDTreeStr state
        return $ Right state


treeFromTagsAndEdgeList :: [a] -> [a] -> DependencyTree a
treeFromTagsAndEdgeList ts es = undefined
    where
        edgesFromList = chunksOf 3 es

toTree :: Show a => a -> DependencyTree a -> Tree String
toTree l (DTNode a t dts) = Node (show  l ++ ": " ++ show  a ++ " " ++ show t) $ map (uncurry toTree) dts

drawDTTree ::  Show a =>  a -> DependencyTree a -> String
drawDTTree a dt  = drawTree $ toTree a dt


{--
fromLabeledEdges :: Eq a => [(a, a, a)] -> [Tag a] -> s -> DependencyTree a
fromLabeledEdges edges tags root = DTNode root tag ns
    where
        fromRoot = filter (\(a, _, _, _) -> a == root) edges
        ns = zip (map (\(_, _, b, _) -> b) fromRoot) $ map (fromLabeledEdges edges tags. (\(_, a, _) -> a)) fromRoot

t = Tag 0 []
edges = [(0,1,100,t), (1,2,200,t), (1,3,200,t), (1,5,500,t), (2,4,400,t)]
--}
