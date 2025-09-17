module Editor.Commands.Handlers.Rules where

import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromJust)
import Data.UUID (fromString)
import Data.UUID.V1 (nextUUID)
import Data.List.Extra (zipFrom, drop1)

import Control.Monad (replicateM)

import CDDB.Rules
import CDDB.CDDB
import CDDB.DeleteNodes
import CDDB.Utils

import Editor.State
import Editor.Utilites
import Editor.Commands.Types

cmdAddRule :: CommandHandler
cmdAddRule state [] CRNothing = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = (newUUID, newRule) : currentRules state}

cmdShowCurrentRules :: CommandHandler
cmdShowCurrentRules state [] CRNothing = do
    mapM_ printFn $ zipFrom 0 (currentRules state)
    return $ Right state
    where
        printFn (idx, r) = do
            print idx
            putStrLn $ ruleDesc r

cmdDeleteRules :: CommandHandler
cmdDeleteRules state [] (CRIntList ns) = if null ns
    then return $ Right state {currentRules = []}
    else return $ Right state {currentRules = deleteItemsByNumbers (currentRules state) ns }

cmdFindRules :: CommandHandler
cmdFindRules state [] (CRUUIDList uuids) = return $ Right state {currentRules = mapMaybe (findCDDBRuleById $ cddb state) uuids }

cmdFilterRules :: CommandHandler
cmdFilterRules state [] (CRTree tree) = return $ Right state -- rulesFound = M.toList $ matchRulesAndFindPaths tree (rules $ cddb state)

cmdRenewRules :: CommandHandler
cmdRenewRules state [] (CRIntList ns) = do
    uuids <- replicateM (length affected) nextUUID
    return $ Right state {currentRules = passed ++ zip (map fromJust uuids) (map snd affected)}
    where
        (affected, passed) = filterByNs (currentRules state) ns

cmdWriteRules :: CommandHandler
cmdWriteRules state [] (CRIntList ns) = do
    return $ Right state {cddb = addRulesToCDDB (cddb state) affected}
    where
        (affected, passed) = filterByNs (currentRules state) ns

cmdWipeRules :: CommandHandler
cmdWipeRules state [] (CRIntList ns) = do
    return $ Right state {cddb = deleteRulesFromCDDB (cddb state) $ map fst affected, currentRules = passed}
    where
        (affected, passed) = filterByNs (currentRules state) ns

cmdSetRulesStop :: CommandHandler
cmdSetRulesStop state [CABool flag] (CRIntList ns) = do
    return $ Right state {currentRules = passed ++ map (mapSnd (\r -> r {stop = flag})) affected}
    where
        (affected, passed) = filterByNs (currentRules state) ns

cmdSetRulesDelNodes :: CommandHandler
cmdSetRulesDelNodes state [] (CRIntList ns) = do
    return $ Right state -- {currentRules = passed ++ map (mapSnd (\r -> r {deletedNodes = (DeleteNodes [])})) affected}
    where
        (affected, passed) = filterByNs (currentRules state) ns

cmdAddRuleItem :: (([a] -> [a]) -> Rule -> Rule) -> CommandHandler
cmdAddRuleItem updater state [CAInt n] (CRString s) = do
    return $ Right state -- {currentRules = passed ++ map (addItem n s) affected}
   where
        addItem n a (ruleId, rule )= (ruleId, updater (insertInNthPosition n a) rule)
        (affected, passed) = filterByNs (currentRules state) [n]

cmdDelRuleItem :: (([a] -> [a]) -> Rule -> Rule) -> CommandHandler
cmdDelRuleItem updater state [CAInt n] (CRIntList ns) = do
    return $ Right state {currentRules = passed ++ map (deleteItem n) affected}
    where
        deleteItem n (ruleId, rule) = (ruleId, updater (deleteNthElement n) rule)
        (affected, passed) = filterByNs (currentRules state) [n]

cmdUpdRuleItem :: (([a] -> [a]) -> Rule -> Rule) -> CommandHandler
cmdUpdRuleItem updater state [CAInt n] (CRString s) = do
    return $ Right state -- {currentRules = map (replaceItem n action) (currentRules state)}
    where
        replaceItem n a (ruleId, rule) = (ruleId, updater (updateNthElement n a) rule)
        (affected, passed) = filterByNs (currentRules state) [n]


updateFacts :: (AddFacts -> AddFacts) -> Rule -> Rule
updateFacts f rule = rule {facts = f (facts rule)}

updateLocals :: (Locals -> Locals) -> Rule -> Rule
updateLocals f rule = rule {locals = f (locals rule)}

updateConditions :: (Conditions -> Conditions) -> Rule -> Rule
updateConditions f rule = rule {conditions = f (conditions rule)}

