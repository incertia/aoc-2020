{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module AOC011 () where

import Control.Lens
  (preview, ix)
import Data.Array
  (Array, array, bounds, indices, (!))
import Data.Foldable
  (toList)
import Data.Maybe
  (fromMaybe)
import Problem
  (Solved(..), Part(..))
import Text.ParserCombinators.ReadPrec
  (get)
import Text.Read
  (readPrec)

data Seat = Floor
          | Empty
          | Occupied
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Read Seat where
  readPrec = get >>= \case
    '.' -> pure Floor
    'L' -> pure Empty
    '#' -> pure Occupied
    c   -> fail $ "got " <> [c]

type Layout = Array (Integer, Integer) Seat

parse :: String -> Layout
parse = mkArr . concat . fmap toArr . zip [0..] . fmap (zip [0..] . fmap (read . pure)) . lines
  where toArr (y, row) = (\(x, s) -> ((x, y), s)) <$> row
        mkArr v = array (fst (head v), fst (last v)) v

next :: Int -> Seat -> Int -> Seat
next n s no = case (s, no == 0, no >= n) of
                (Empty, True, _) -> Occupied
                (Occupied, _, True) -> Empty
                _ -> s

go :: Layout -> Layout
go a = array (bounds a) (f <$> indices a)
  where f i = (i,) $ next 4 (a ! i) (no i)
        no (x, y) = occupied $ fromMaybe Floor . flip preview a . ix <$>
                      [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
                       (x, y - 1),                 (x, y + 1),
                       (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

go2 :: Layout -> Layout
go2 a = array (bounds a) (f <$> indices a)
  where f i = (i,) $ next 5 (a ! i) (no i)
        no i = occupied $ fromMaybe Floor . flip preview a . ix . first i <$>
                 [(-1, -1), (-1, 0), (-1, 1),
                  ( 0, -1),          ( 0, 1),
                  ( 1, -1), ( 1, 0), ( 1, 1)]
        first i d = case preview (ix i') a of
                      Just z -> if z /= Floor then i' else first i' d
                      Nothing -> i'
          where i' = (fst i + fst d, snd i + snd d)
occupied :: [Seat] -> Int
occupied = length . filter (==Occupied)

countFinal :: (Layout -> Layout) -> Layout -> Int
countFinal g = occupied . toList . f . zip [0 :: Integer ..] . iterate g
  where f (x:y:z) = if snd x == snd y then snd x else f (y:z)
        f _ = error "impossible"

instance Solved 11 'PartA where
  solve = show . countFinal go . parse
instance Solved 11 'PartB where
  solve = show . countFinal go2 . parse
