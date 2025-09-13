module Editor.Commands.Handlers.DependencyTree where

import Data.Maybe (fromMaybe)

import Editor.State
import Editor.Commands.Types

import Support.Support

cmdBuildTree :: CommandHandler
cmdBuildTree state [] (CRIntList tags) = do
    edges <- buildDependencyTree (supportEngine state) tags
    print edges
    return $ Right state {dependencyTree = edges}

cmdDescribeRel :: CommandHandler
cmdDescribeRel state [] (CRIntList depRels) = do
    tagDescrs <- mapM (describeRel (supportEngine state)) depRels
    print tagDescrs
    return $ Right state

cmdDescribeCurrentTree :: CommandHandler
cmdDescribeCurrentTree state [] (CRIntList depRels) = case (dependencyTree state) of
    Nothing -> return $ Left ""
    Just depRels -> do
        tagDescrs <- mapM (describeRel (supportEngine state)) depRels
        print tagDescrs
        return $ Right state {dependencyTreeDescription = Just $ map (fromMaybe ["?"]) tagDescrs}

cmdShowCurrentTree :: CommandHandler
cmdShowCurrentTree state [] CRNothing = case (dependencyTree state) of
    Nothing -> return $ Left "No current tree exists yet."
    Just tags -> do
        print tags
        print $ dependencyTreeDescription state
        return $ Right state
