{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AOC002 () where

import Control.Applicative
  (many)
import Control.Lens
  ((^?), ix)
import Data.Bits
  (xor)
import Data.Char
  (isDigit)
import Problem
  (Solved(..), Part(..))
import Text.ParserCombinators.ReadP
  (get, char, munch1, skipSpaces)
import Text.ParserCombinators.ReadPrec
  (lift)
import Text.Read
  (Read(..))

data Password = Password Int Int Char String
  deriving (Eq, Show)

instance Read Password where
  readPrec = lift $ do
    min' <- read <$> munch1 isDigit
    _ <- char '-'
    max' <- read <$> munch1 isDigit
    skipSpaces
    c   <- get
    _ <- char ':'
    skipSpaces
    pw  <- many get
    pure $ Password min' max' c pw

validateA :: Password -> Bool
validateA (Password l h c p) = n >= l && n <= h
  where n = length $ filter (==c) p

validateB :: Password -> Bool
validateB (Password l h c p) = first `xor` second
  where first = p ^? ix (l - 1) == Just c
        second = p ^? ix (h - 1) == Just c

instance Solved 2 'PartA where
  solve = show . length . filter validateA . fmap read . lines
instance Solved 2 'PartB where
  solve = show . length . filter validateB . fmap read . lines
