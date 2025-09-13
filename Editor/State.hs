module Editor.State where

import CDDB.Types (Name)
import CDDB.CDDB
import CDDB.Rules

import Support.Support

import Editor.Settings

data ProgramState = ProgramState {
        settings :: Settings,
        cddb :: CDDB,
        currentRules :: [(RuleId, Rule)],
        isNotSaved :: Bool,
        currentTemplate :: Maybe Name,
        supportEngine :: Handle,
        taggedSentence :: Maybe [Int],
        dependencyTree :: Maybe [Int]
    }

initialProgramState :: Settings -> IO ProgramState
initialProgramState settings = do
    se <- initEngine
    return $ ProgramState
        {
            settings = settings,
            cddb = emptyCDDB,
            currentRules = [],
            isNotSaved = True,
            currentTemplate = Nothing,
            supportEngine = se,
            taggedSentence = Nothing,
            dependencyTree = Nothing
        }
