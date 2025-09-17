module Editor.Utilites where

import Data.List.Extra (zipFrom, drop1, sort)

deleteItemsByNumbers :: [a] -> [Int] -> [a]
deleteItemsByNumbers items ns = foldl (flip deleteN) items (sort ns)

deleteN :: Int -> [a] -> [a]
deleteN n items = before ++ drop1 after
    where
        (before, after) = splitAt n items

spanByNs :: [b] -> [Int] -> ([b], [b])
spanByNs items ns = (map snd ein, map snd eout)
    where
        (ein, eout) = span ((`elem` ns) . fst) $ zipFrom 0 items

filterByNs :: [a2] -> [Int] -> ([a2], [a2])
filterByNs rs ns = if null ns then (rs, []) else spanByNs rs ns
