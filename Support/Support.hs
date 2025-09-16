{-# LANGUAGE CApiFFI #-}

module Support.Support where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Array

foreign import capi "Support.h parse" parse' :: CString -> CString -> IO CBool
foreign import capi "Support.h saveSentences" saveSentences' :: CString -> IO CBool
foreign import capi "Support.h loadSentences" loadSentences' :: CString -> IO CBool
foreign import capi "Support.h saveEncoder" saveEncoder' :: CString -> IO CBool
foreign import capi "Support.h loadEncoder" loadEncoder' :: CString -> IO CBool
foreign import capi "Support.h trainTagger" trainTagger' :: CFloat -> IO CBool
foreign import capi "Support.h tag" tag' :: Ptr CString -> CULong -> Ptr CUShort -> IO CBool
foreign import capi "Support.h describeTag" describeTag' :: CULong -> Ptr CString -> Ptr CULong -> IO CBool
foreign import capi "Support.h saveTagger" saveTagger' :: CString -> IO CBool
foreign import capi "Support.h loadTagger" loadTagger' :: CString -> IO CBool
foreign import capi "Support.h saveTreeBuilder" saveTreeBuilder' :: CString -> IO CBool
foreign import capi "Support.h loadTreeBuilder" loadTreeBuilder' :: CString -> IO CBool
foreign import capi "Support.h buildDependencyTree" buildDependencyTree' :: Ptr CUShort -> CULong -> Ptr CUShort -> IO CBool
foreign import capi "Support.h describeRel" describeRel' :: CULong -> Ptr CString -> Ptr CULong -> IO CBool

parsePath :: FilePath -> String -> IO Bool
parsePath path parser = do
    cpath <- newCString path
    cparser <- newCString parser
    res <- parse' cpath cparser
    return $ toBool res

saveSentences :: FilePath -> IO Bool
saveSentences path = do
    cpath <- newCString path
    res <- saveSentences' cpath
    return $ toBool res

loadSentences :: FilePath -> IO Bool
loadSentences path = do
    cpath <- newCString path
    res <- loadSentences' cpath
    return $ toBool res

saveEncoder :: FilePath -> IO Bool
saveEncoder path = do
    cpath <- newCString path
    res <- saveEncoder' cpath
    return $ toBool res

loadEncoder :: FilePath -> IO Bool
loadEncoder path = do
    cpath <- newCString path
    res <- loadEncoder' cpath
    return $ toBool res

trainTagger :: Float -> IO Bool
trainTagger sf = do
    res <- trainTagger' (realToFrac sf)
    return $ toBool res

saveTagger :: FilePath -> IO Bool
saveTagger path = do
    cpath <- newCString path
    res <- saveTagger' cpath
    return $ toBool res

loadTagger :: FilePath -> IO Bool
loadTagger path = do
    cpath <- newCString path
    res <- loadTagger' cpath
    return $ toBool res

tag :: [String] -> IO (Maybe [Int])
tag ss = do
    css <- mallocArray size
    cs <- mapM newCString ss
    pokeArray css cs
    ts <- mallocArray size
    res <- tag' css (toEnum size) ts
    if toBool res then do
        tags <- peekArray size ts
        return $ Just $ map fromEnum tags
    else return Nothing
    where
        size = length ss

describeTag :: Int -> IO (Maybe [String])
describeTag tag = do
    cs <- newCString ""
    p <- new cs
    plen <- new 0
    res <- describeTag' (toEnum tag) p plen
    if toBool res then do
        size <- peek plen
        ss <- peekArray (fromEnum size) p
        ss' <- mapM peekCString ss
        return $ Just ss'
    else return Nothing

saveTreeBuilder :: FilePath -> IO Bool
saveTreeBuilder path = do
    cpath <- newCString path
    res <- saveTreeBuilder' cpath
    return $ toBool res

loadTreeBuilder :: FilePath -> IO Bool
loadTreeBuilder path = do
    cpath <- newCString path
    res <- loadTreeBuilder' cpath
    return $ toBool res

buildDependencyTree :: [Int] -> IO (Maybe [Int])
buildDependencyTree ss = do
    ts <- mallocArray size
    es <- mallocArray (3 * size)
    res <- tag' ts (toEnum size) es
    if toBool res then do
        edges <- peekArray (3 * size) es
        return $ Just $ map fromEnum edges
    else return Nothing
    where
        size = length ss

describeRel :: Int -> IO (Maybe [String])
describeRel tag = do
    cs <- newCString ""
    p <- new cs
    plen <- new 0
    res <- describeRel' (toEnum tag) p plen
    if toBool res then do
        size <- peek plen
        ss <- peekArray (fromEnum size) p
        ss' <- mapM peekCString ss
        return $ Just ss'
    else return Nothing
