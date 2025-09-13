module Editor.Commands.Handlers.DependencyTree where

import Data.Maybe (fromMaybe)

import Editor.State
import Editor.Commands.Types
import Editor.Commands.Handlers

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
