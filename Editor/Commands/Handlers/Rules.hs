module Editor.Commands.Handlers.Rules where

import Data.Maybe (mapMaybe, fromJust)
import Data.UUID.V1 (nextUUID)
import Data.List.Extra (zipFrom)

import Control.Monad (replicateM)

import CDDB.Rules
import CDDB.CDDB
--import CDDB.DeleteNodes
import CDDB.Utils

import Editor.State
import Editor.Utilites
import Editor.Commands.Types

cmdAddRule :: CommandHandler
cmdAddRule state [] CRNothing = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = (newUUID, newRule) : currentRules state}
cmdAddRule _ _ _ = undefined

cmdShowCurrentRules :: CommandHandler
cmdShowCurrentRules state [] CRNothing = do
    mapM_ printFn $ zipFrom (0 :: Int) (currentRules state)
    return $ Right state
    where
        printFn (idx, r) = do
            print idx
            putStrLn $ ruleDesc r
cmdShowCurrentRules _ _ _ = undefined

cmdDeleteRules :: CommandHandler
cmdDeleteRules state [] (CRIntList ns) = if null ns
    then return $ Right state {currentRules = []}
    else return $ Right state {currentRules = deleteItemsByNumbers (currentRules state) ns }
cmdDeleteRules _ _ _ = undefined

cmdFindRules :: CommandHandler
cmdFindRules state [] (CRUUIDList uuids) = return $ Right state {currentRules = mapMaybe (findCDDBRuleById $ cddb state) uuids }
cmdFindRules _ _ _ = undefined

cmdFilterRules :: CommandHandler
cmdFilterRules state [] (CRTree _) = return $ Right state -- rulesFound = M.toList $ matchRulesAndFindPaths tree (rules $ cddb state)
cmdFilterRules _ _ _ = undefined

cmdRenewRules :: CommandHandler
cmdRenewRules state [] (CRIntList ns) = do
    uuids <- replicateM (length affected) nextUUID
    return $ Right state {currentRules = passed ++ zip (map fromJust uuids) (map snd affected)}
    where
        (affected, passed) = filterByNs (currentRules state) ns
cmdRenewRules _ _ _ = undefined

cmdWriteRules :: CommandHandler
cmdWriteRules state [] (CRIntList ns) = do
    return $ Right state {cddb = addRulesToCDDB (cddb state) affected}
    where
        (affected, _) = filterByNs (currentRules state) ns
cmdWriteRules _ _ _ = undefined

cmdWipeRules :: CommandHandler
cmdWipeRules state [] (CRIntList ns) = do
    return $ Right state {cddb = deleteRulesFromCDDB (cddb state) $ map fst affected, currentRules = passed}
    where
        (affected, passed) = filterByNs (currentRules state) ns
cmdWipeRules _ _ _ = undefined

cmdSetRulesStop :: CommandHandler
cmdSetRulesStop state [CABool flag] (CRIntList ns) = do
    return $ Right state {currentRules = passed ++ map (mapSnd (\r -> r {stop = flag})) affected}
    where
        (affected, passed) = filterByNs (currentRules state) ns
cmdSetRulesStop _ _ _ = undefined

cmdSetRulesDelNodes :: CommandHandler
cmdSetRulesDelNodes state [] (CRIntList _) = do
    return $ Right state -- {currentRules = passed ++ map (mapSnd (\r -> r {deletedNodes = (DeleteNodes [])})) affected}
    where
        --(affected, passed) = filterByNs (currentRules state) ns
cmdSetRulesDelNodes _ _ _ = undefined

cmdAddRuleItem :: (([a] -> [a]) -> Rule -> Rule) -> CommandHandler
cmdAddRuleItem _ state [CAInt _] (CRString _) = do
    return $ Right state -- {currentRules = passed ++ map (addItem n s) affected}
   where
        --addItem n a (ruleId, rule )= (ruleId, updater (insertInNthPosition n a) rule)
        --(affected, passed) = filterByNs (currentRules state) [n]
cmdAddRuleItem _ _ _ _ = undefined

cmdDelRuleItem :: (([a] -> [a]) -> Rule -> Rule) -> CommandHandler
cmdDelRuleItem _ state [CAInt _] (CRIntList _) = do
    return $ Right state --{currentRules = passed ++ map (deleteItem narg) affected}
    where
        --deleteItem n (ruleId, rule) = (ruleId, updater (deleteNthElement n) rule)
        --(affected, passed) = filterByNs (currentRules state) [narg]
cmdDelRuleItem _ _ _ _ = undefined

cmdUpdRuleItem :: (([a] -> [a]) -> Rule -> Rule) -> CommandHandler
cmdUpdRuleItem _ state [CAInt _] (CRString _) = do
    return $ Right state -- {currentRules = map (replaceItem n action) (currentRules state)}
    where
        --replaceItem n a (ruleId, rule) = (ruleId, updater (updateNthElement n a) rule)
        --(affected, passed) = filterByNs (currentRules state) [n]
cmdUpdRuleItem _ _ _ _ = undefined


updateFacts :: (AddFacts -> AddFacts) -> Rule -> Rule
updateFacts f rule = rule {facts = f (facts rule)}

updateLocals :: (Locals -> Locals) -> Rule -> Rule
updateLocals f rule = rule {locals = f (locals rule)}

updateConditions :: (Conditions -> Conditions) -> Rule -> Rule
updateConditions f rule = rule {conditions = f (conditions rule)}

