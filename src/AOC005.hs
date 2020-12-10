{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AOC005 () where

import Control.Applicative
  (many)
import Data.Foldable
  (foldl')
import Problem
  (Solved(..), Part(..))
import Text.Read
  (Read(..), get, pfail)

data Row = F | B
data Seat = L | R
data BoardingPass = BoardingPass [Row] [Seat]

class BS a where
  bs :: a -> Bool

instance Read Row where
  readPrec =
    get >>= \case
      'F' -> pure F
      'B' -> pure B
      _   -> pfail

instance Read Seat where
  readPrec =
    get >>= \case
      'L' -> pure L
      'R' -> pure R
      _   -> pfail

instance BS Row where
  bs F = False
  bs B = True

instance BS Seat where
  bs L = False
  bs R = True

instance Read BoardingPass where
  readPrec = BoardingPass <$> many readPrec <*> many readPrec

search :: BS a => [a] -> Int
search = foldl' (\a b-> 2 * a + b) 0 . fmap (fromEnum . bs)

solver :: Part -> String -> String
solver p i = show $ case p of
               PartA -> hi
               PartB -> sum [lo..hi] - sum ids
  where getId (BoardingPass r s) = 8 * search r + search s
        hi = maximum ids
        lo = minimum ids
        ids = fmap (getId . read) . lines $ i

instance Solved 5 'PartA where
  solve = solver PartA
instance Solved 5 'PartB where
  solve = solver PartB
