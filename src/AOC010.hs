{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AOC010 () where

import Control.Arrow
  ((&&&))
import Control.Lens
  (at, (?~))
import Data.Foldable
  (foldl')
import Data.Function
  ((&))
import Data.List
  (sort)
import Data.Map.Strict
  ((!), fromList, findWithDefault)
import Problem
  (Solved(..), Part(..))

jolts :: String -> [Integer]
jolts inp = let x = fmap read . lines $ inp in sort $ maximum x + 3 : x

ways :: [Integer] -> Integer
ways v = foldl' f (fromList [(0, 1)]) v ! last v
  where f m a = m & at a ?~ (sum $ flip (findWithDefault 0) m <$> [a - 1, a - 2, a - 3])

instance Solved 10 'PartA where
  solve = show . uncurry (*) . ((length . filter (==1)) &&& (length . filter (==3)))
               . (\v -> zipWith (-) v (0:v)) . jolts
instance Solved 10 'PartB where
  solve = show . ways . jolts
