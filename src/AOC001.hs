{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AOC001 () where

import Control.Applicative
  ((<|>))
import Data.Maybe
  (fromJust)
import Problem
  (Solved(..), Part(..))

try_sums :: Integer -> Integer -> [Integer] -> [Integer] -> Maybe Integer
try_sums t 0 as _ = if sum as == t then Just (product as) else Nothing
try_sums _ _ _ [] = Nothing
try_sums t n as (b:bs) = try_sums t (n - 1) (b:as) bs <|> try_sums t n as bs

instance Solved 1 'PartA where
  solve = show . fromJust . try_sums 2020 2 [] . fmap read . lines
instance Solved 1 'PartB where
  solve = show . fromJust . try_sums 2020 3 [] . fmap read . lines
