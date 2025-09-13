module Editor.Commands.Handlers.Runner where

import Text.Read (readEither)

import CDDB.Logging
import CDDB.Runner.Runner
import CDDB.Runner.Context

import Editor.State
import Editor.Settings
import Editor.Commands.Types

cmdRunTree :: CommandHandler
cmdRunTree state [] (CRString s) =  case readEither s of
    Left err -> return $ Left err
    Right tree -> if null results
        then return $ Left "Nothing happened..."
        else do
            mapM_ printResult results
            return $ Right state
        where
            results = applyTree (cddb state) tree (maxRecursionDepth $ settings state)
            printResult result = do
                putStr "Score:"
                print (accumulatedScore result)
                putStrLn "Tree after run:"
                print (currentTree result)
                putStrLn "Maximum recursion depth:"
                print (recursionDepth result)
                putStrLn "Accumulated knowledge:"
                print (accumulatedKnowledge result)
                putStrLn "Logs:"
                printLogs (workingLog result)
                return ()

cmdRunCurrentTree :: CommandHandler
cmdRunCurrentTree state [] CRNothing = undefined

cmdSetCurrentTree :: CommandHandler
cmdSetCurrentTree state [] (CRTree t) = undefined
