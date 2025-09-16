module Editor.State where

import CDDB.Types (Name)
import CDDB.CDDB
import CDDB.Rules
import CDDB.Syntax.Tag
import CDDB.Syntax.DependencyTree

import Editor.Settings

data ProgramState = ProgramState {
        settings :: Settings,
        cddb :: CDDB,
        currentRules :: [(RuleId, Rule)],
        isNotSaved :: Bool,
        currentTemplate :: Maybe Name,

        currentTaggedSentence :: Maybe (Tags Int),
        currentTaggedSentenceStr :: Maybe (Tags String),
        currentDTree :: Maybe (DependencyTree Int),
        currentDTreeStr :: Maybe (DependencyTree String)
    }

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState
        {
            settings = settings,
            cddb = emptyCDDB,
            currentRules = [],
            isNotSaved = True,
            currentTemplate = Nothing,

            currentTaggedSentence = Nothing,
            currentTaggedSentenceStr = Nothing,
            currentDTree  = Nothing,
            currentDTreeStr = Nothing
        }
