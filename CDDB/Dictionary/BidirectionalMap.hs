module CDDB.Dictionary.BidirectionalMap where

import qualified Data.Set as S
import qualified Data.Vector.Strict as V

-- Ordered vector
data BidirectionalMap a = BidirectionalMap !(V.Vector a)

instance Show a => Show (BidirectionalMap a) where
    show bm = show $ toList bm

size :: BidirectionalMap a -> Int
size (BidirectionalMap v) = V.length v

newBidirectionalMap :: Monoid a => Int -> BidirectionalMap a
newBidirectionalMap n = BidirectionalMap $ V.replicate n mempty

toList :: BidirectionalMap a -> [a]
toList (BidirectionalMap v) = V.toList v

fromList :: Ord a => [a] -> BidirectionalMap a
fromList xs = fromSet $ S.fromList xs

fromSet :: Ord a => S.Set a -> BidirectionalMap a
fromSet s = BidirectionalMap $ V.unfoldrN (S.size s) S.minView s

findItem :: Ord a => BidirectionalMap a -> a -> Maybe Int
findItem (BidirectionalMap v) x = binarySearch 0 (V.length v)
    where
        binarySearch lo hi | lo >= hi = Nothing
        binarySearch lo hi = case compare x x' of
            LT -> binarySearch lo mid
            EQ -> Just mid
            GT -> binarySearch (mid + 1) hi
            where
                mid = (lo + hi) `div` 2
                x' = v V.! mid

lookupId :: BidirectionalMap a -> Int -> a
lookupId (BidirectionalMap v) i = v V.! i
