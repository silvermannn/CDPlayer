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

        currentTaggedSentence :: Maybe [Tag Int],
        currentTaggedSentenceStr :: Maybe [Tag String],
        currentDTree :: Maybe (DependencyTree Int),
        currentDTreeStr :: Maybe (DependencyTree String),
        --------------------------
        taggedSentence :: Maybe [Int],
        taggedSentenceDescription :: Maybe [[String]],
        dependencyTree :: Maybe [Int],
        dependencyTreeDescription :: Maybe [[String]]
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
            currentDTreeStr = Nothing,
            --------------------------
            taggedSentence = Nothing,
            taggedSentenceDescription = Nothing,
            dependencyTree = Nothing,
            dependencyTreeDescription = Nothing
        }
