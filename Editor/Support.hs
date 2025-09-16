module Editor.Support where

import CDDB.Syntax.Tag
import CDDB.Syntax.DependencyTree

import Support.Support

type Result a = IO (Maybe a)

tagSentence :: [String] -> Result (Tags Int)
tagSentence ws = do
    tags <- tag ws
    return Nothing

describeTags :: Tags Int -> Result (Tags String)
describeTags tags = do
    return Nothing

buidTree :: Tags Int -> Result (DependencyTree Int)
buidTree tags = do
    -- edges <- buildDependencyTree tags
    return Nothing

describeTree :: DependencyTree Int -> Result (DependencyTree String)
describeTree tree = do
    return Nothing
