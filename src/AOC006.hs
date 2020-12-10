{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AOC006 () where

import Data.List
  (group, sort)
import Data.List.Split
  (splitOn)
import Problem
  (Solved(..), Part(..))

answers :: String -> [[String]]
answers = splitOn [""] . lines

solver :: Part -> String -> String
solver p i = show . sum $ f <$> answers i
  where f a = length . filter (g a) . group . sort . concat $ a
        g a = case p of
                PartA -> const True
                PartB -> (==(length a)) . length

instance Solved 6 'PartA where
  solve = solver PartA
instance Solved 6 'PartB where
  solve = solver PartB
