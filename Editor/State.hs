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

        currentCTaggedSentence :: Maybe [Int],
        currentTaggedSentence :: Maybe (Tags Int),
        currentDTree :: Maybe (DependencyTree Int)
    }

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState
        {
            settings = settings,
            cddb = emptyCDDB,
            currentRules = [],
            isNotSaved = True,
            currentTemplate = Nothing,

            currentCTaggedSentence = Nothing,
            currentTaggedSentence = Nothing,
            currentDTree  = Nothing
        }
