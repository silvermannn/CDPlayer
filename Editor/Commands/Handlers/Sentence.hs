module Editor.Commands.Handlers.Sentence where

import Data.Maybe (catMaybes)
import Data.List.Extra (chunksOf)

import Editor.State
import Editor.Commands.Types
import Editor.Support

import CDDB.Syntax.DependencyTree

cmdTagSentence :: CommandHandler
cmdTagSentence state [] (CRStringList sentence) = do
    ctags <- tagSentence sentence
    print ctags
    case ctags of
        Just ctags' -> do
            tags <- mapM describeCompoundTag ctags'
            print tags
            return $ Right state {currentCTaggedSentence = ctags, currentTaggedSentence = Just $ catMaybes tags}
        _ -> return $ Left "error"

cmdShowCurrentSentence :: CommandHandler
cmdShowCurrentSentence state [] CRNothing = case (currentTaggedSentence state) of
    Nothing -> return $ Left "No current sentence tagged yet."
    Just tags -> do
        print (currentCTaggedSentence state)
        print tags
        dtags <- describeTags tags
        print dtags
        return $ Right state
