module Editor.Commands.Handlers.Templates where

import Data.List.Extra (zipFrom)

import CDDB.CDDB
import CDDB.Templates

import Editor.State
import Editor.Commands.Types

cmdAddTemplate :: CommandHandler
cmdAddTemplate state [CAString n] (CRStringList fs) = do
    return $ Right state {cddb = addTemplatesToCDDB (cddb state) [PrimitiveTemplate n fs]}
cmdAddTemplate _ _ _ = undefined

cmdShowTemplate :: CommandHandler
cmdShowTemplate state [CAString _] CRNothing = do
    mapM_ printFn $ zipFrom (0 :: Int) (unpackTemplates $ templates $ cddb state)
    return $ Right state
    where
        printFn (idx, r) = do
            print idx
            putStrLn $ templateDesc r
cmdShowTemplate _ _ _ = undefined

cmdDeleteTemplate :: CommandHandler
cmdDeleteTemplate state [CAString n] CRNothing = do
    return $ Right state {cddb = deleteTemplatesFromCDDB (cddb state) [n]}
cmdDeleteTemplate _ _ _ = undefined
