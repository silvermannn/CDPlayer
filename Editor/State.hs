module Editor.State where

import CDDB.Types (Name)
import CDDB.CDDB
import CDDB.Rules

import Editor.Settings
import Editor.Sentence


data ProgramState = ProgramState {
        settings :: Settings,
        cddb :: CDDB,
        currentRules :: [(RuleId, Rule)],
        isNotSaved :: Bool,
        currentTemplate :: Maybe Name,
        currentSentences :: [CurrentSentence]
    }

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState
        {
            settings = settings,
            cddb = emptyCDDB,
            currentRules = [],
            isNotSaved = True,
            currentTemplate = Nothing,
            currentSentences = []
        }
