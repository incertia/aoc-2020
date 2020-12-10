{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AOC009 () where

import Control.Arrow
  ((&&&), (***))
import Data.Vector
  (Vector, fromList, iterateN, (!), slice)
import Problem
  (Solved(..), Part(..))

import qualified Data.Vector as V
  (filter, head, tail, take, length, last, scanl)

solveA :: Vector Integer -> Integer
solveA = V.last . V.head . V.filter (not . isValid) . questions 25

solveB :: Vector Integer -> Integer
solveB nums = head $ uncurry (+) . (minimum &&& maximum) . subl nums <$> regions
  where target = solveA nums
        ps = V.scanl (+) 0 nums
        regions = filter ((==(-target)) . uncurry (-) . ((ps!) *** (ps!))) (pairs l)
        subl v (i, j) = slice i (j - i) v
        l = length nums

questions :: Int -> Vector a -> Vector (Vector a)
questions l v = fmap (V.take (l + 1)) . iterateN (V.length v + 1 - l) V.tail $ v

pairs :: Int -> [(Int, Int)]
pairs l = [(i, j) | i <- [0..l], j <- [i + 1..l]]

isValid :: Vector Integer -> Bool
isValid v = if null v then False
            else q - x `elem` pre || isValid vs
  where l = V.length v - 1
        (x, (vs, q)) = (V.head &&& V.tail &&& V.last) v
        pre = V.take (l - 1) vs

instance Solved 9 'PartA where
  solve = show . solveA . fmap read . fromList . lines
instance Solved 9 'PartB where
  solve = show . solveB . fmap read . fromList . lines
