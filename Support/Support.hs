{-# LANGUAGE CApiFFI #-}

module Support.Support where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Array

newtype {-# CTYPE "Support.h" "Handle" #-} Handle = Handle (Ptr ())

foreign import capi "Support.h init" initEngine :: IO Handle
foreign import capi "Support.h clear" clearEngine :: Handle -> IO ()
foreign import capi "Support.h parse" parse' :: Handle -> CString -> CString -> IO CBool
foreign import capi "Support.h saveSentences" saveSentences' :: Handle -> CString -> IO CBool
foreign import capi "Support.h loadSentences" loadSentences' :: Handle -> CString -> IO CBool
foreign import capi "Support.h saveEncoder" saveEncoder' :: Handle -> CString -> IO CBool
foreign import capi "Support.h loadEncoder" loadEncoder' :: Handle -> CString -> IO CBool
foreign import capi "Support.h trainTagger" trainTagger' :: Handle -> CFloat -> IO CBool
foreign import capi "Support.h tag" tag' :: Handle -> Ptr CString -> CULong -> Ptr CUShort -> IO CBool
foreign import capi "Support.h describeTag" describeTag' :: Handle -> CULong -> Ptr CString -> Ptr CULong -> IO CBool
foreign import capi "Support.h saveTagger" saveTagger' :: Handle -> CString -> IO CBool
foreign import capi "Support.h loadTagger" loadTagger' :: Handle -> CString -> IO CBool
foreign import capi "Support.h saveTreeBuilder" saveTreeBuilder' :: Handle -> CString -> IO CBool
foreign import capi "Support.h loadTreeBuilder" loadTreeBuilder' :: Handle -> CString -> IO CBool
foreign import capi "Support.h release" release' :: Ptr a -> IO ()

parsePath :: Handle -> FilePath -> String -> IO Bool
parsePath h path parser = do
    cpath <- newCString path
    cparser <- newCString parser
    res <- parse' h cpath cparser
    return $ toBool res

saveSentences :: Handle -> FilePath -> IO Bool
saveSentences h path = do
    cpath <- newCString path
    res <- saveSentences' h cpath
    return $ toBool res

loadSentences :: Handle -> FilePath -> IO Bool
loadSentences h path = do
    cpath <- newCString path
    res <- loadSentences' h cpath
    return $ toBool res

saveEncoder :: Handle -> FilePath -> IO Bool
saveEncoder h path = do
    cpath <- newCString path
    res <- saveEncoder' h cpath
    return $ toBool res

loadEncoder :: Handle -> FilePath -> IO Bool
loadEncoder h path = do
    cpath <- newCString path
    res <- loadEncoder' h cpath
    return $ toBool res

trainTagger :: Handle -> Float -> IO Bool
trainTagger h sf = do
    res <- trainTagger' h (realToFrac sf)
    return $ toBool res

saveTagger :: Handle -> FilePath -> IO Bool
saveTagger h path = do
    cpath <- newCString path
    res <- saveTagger' h cpath
    return $ toBool res

loadTagger :: Handle -> FilePath -> IO Bool
loadTagger h path = do
    cpath <- newCString path
    res <- loadTagger' h cpath
    return $ toBool res

tag :: Handle -> [String] -> IO (Maybe [Int])
tag h ss = do
    css <- mallocArray size
    cs <- mapM newCString ss
    pokeArray css cs
    ts <- mallocArray size
    res <- tag' h css (toEnum size) ts
    if toBool res then do
        tags <- peekArray size ts
        return $ Just $ map fromEnum tags
    else return Nothing
    where
        size = length ss

describeTag :: Handle -> Int -> IO (Maybe [String])
describeTag h tag = do
    cs <- newCString ""
    p <- new cs
    plen <- new 0
    res <- describeTag' h (toEnum tag) p plen
    if toBool res then do
        size <- peek plen
        ss <- peekArray (fromEnum size) p
        ss' <- mapM peekCString ss
        return $ Just ss'
    else return Nothing

saveTreeBuilder :: Handle -> FilePath -> IO Bool
saveTreeBuilder h path = do
    cpath <- newCString path
    res <- saveTreeBuilder' h cpath
    return $ toBool res

loadTreeBuilder :: Handle -> FilePath -> IO Bool
loadTreeBuilder h path = do
    cpath <- newCString path
    res <- loadTreeBuilder' h cpath
    return $ toBool res

