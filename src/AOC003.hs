{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AOC003 () where

import Data.Array
  (Array, (!), listArray, bounds)
import Problem
  (Solved(..), Part(..))
import Text.Read
  (Read(..), get, pfail)

data Space = Empty | Tree
  deriving (Show, Eq, Enum, Bounded)

instance Read Space where
  readPrec =
    get >>= \case
      '.' -> pure Empty
      '#' -> pure Tree
      _   -> pfail

travel :: (Int, Int) -> (Int, Int) -> Int -> Array (Int, Int) Space -> Int
travel (y, x) (dy, dx) n v =
  if y' > h then n
  else case v ! (y', x') of
    Empty -> travel (y', x') (dy, dx) n v
    Tree  -> travel (y', x') (dy, dx) (n + 1) v
  where (_, (h, w)) = bounds v
        (y', x') = (y + dy, (x + dx) `mod` (w + 1))

solver :: Part -> String -> String
solver p i = show . product $ go <$> slopes
  where h = length i'
        w = length v `div` h
        i' = lines i
        v = concat $ fmap (read . pure) <$> i'
        geo = listArray ((0, 0), (h - 1, w - 1)) v
        go s = travel (0, 0) s 0 geo
        slopes = case p of
                   PartA -> [(1, 3)]
                   PartB -> [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

instance Solved 3 'PartA where
  solve = solver PartA
instance Solved 3 'PartB where
  solve = solver PartB
