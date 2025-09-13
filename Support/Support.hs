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
foreign import capi "Support.h load" load' :: Handle -> CString -> CString -> IO CBool
foreign import capi "Support.h save" save' :: Handle -> CString -> IO CBool
foreign import capi "Support.h tag" tag' :: Handle -> Ptr CString -> CULong -> Ptr CUShort -> IO CBool
foreign import capi "Support.h describeTag" describeTag' :: Handle -> CULong -> Ptr CString -> Ptr CULong -> IO CBool
foreign import capi "Support.h release" release' :: Ptr a -> IO ()

loadPath :: Handle -> FilePath -> String -> IO Bool
loadPath h path parser = do
    cpath <- newCString path
    cparser <- newCString parser
    res <- load' h cpath cparser
    return $ toBool res

savePath :: Handle -> FilePath -> IO Bool
savePath h path = do
    cpath <- newCString path
    res <- save' h cpath
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
