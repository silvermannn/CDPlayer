module Editor.Commands.Handlers.DependencyTree where

import Data.Maybe (fromMaybe)
import Data.List.Extra (chunksOf)
import Data.Tree

import CDDB.Syntax.DependencyTree

import Editor.State
import Editor.Commands.Types
import Editor.Support

cmdBuildTree :: CommandHandler
cmdBuildTree state [] CRNothing =  case currentCTaggedSentence state of
    Nothing -> return $ Left "No current sentence tagged yet."
    Just tags -> do
        tree <- buidTree tags
        print tree
        return $ Right state {currentDTree = tree}

cmdShowCurrentTree :: CommandHandler
cmdShowCurrentTree state [] CRNothing = case currentDTree state of
    Nothing -> return $ Left "No current tree exists yet."
    Just tree -> do
        print tree
        stree <- describeTree tree
        print $ drawDTTree "root" <$> stree
        return $ Right state
    where
        toTree :: Show a => a -> DependencyTree a -> Tree String
        toTree l (DTNode a t dts) = Node (show  l ++ ": " ++ show  a ++ " " ++ show t) $ map (uncurry toTree) dts

        drawDTTree ::  Show a =>  a -> DependencyTree a -> String
        drawDTTree a dt  = drawTree $ toTree a dt
