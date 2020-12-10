{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module AOC004 () where

import Control.Applicative
  (many)
import Control.Lens
  (view, anyOf, allOf, filtered)
import Control.Lens.TH
  (makeLenses)
import Data.Char
  (isDigit, ord)
import Data.List.Split
  (splitOn)
import Problem
  (Solved(..), Part(..))
import Text.ParserCombinators.ReadP
  (get, char, manyTill, skipSpaces)
import Text.ParserCombinators.ReadPrec
  (lift)
import Text.Read
  (Read(..), readMaybe)

data Field = Field { _fname :: String, _fvalue :: String }
  deriving (Show, Eq)
makeLenses ''Field

type Passport = [Field]

instance Read Field where
  readPrec = lift $ do
    skipSpaces
    f <- manyTill get (char ':')
    v <- many get
    pure $ Field f v

hasField :: String -> Passport -> Bool
hasField f = anyOf (traverse . fname) (==f)

validateField :: String -> (String -> Bool) -> Passport -> Bool
validateField f g = allOf (traverse . filtered ((==f) . view fname) . fvalue) g

byr :: String -> Bool
byr y' = case readMaybe @Int y' of
           Just y  -> length y' == 4 && y >= 1920 && y <= 2002
           Nothing -> False

iyr :: String -> Bool
iyr y' = case readMaybe @Int y' of
           Just y  -> length y' == 4 && y >= 2010 && y <= 2020
           Nothing -> False

eyr :: String -> Bool
eyr y' = case readMaybe @Int y' of
           Just y  -> length y' == 4 && y >= 2020 && y <= 2030
           Nothing -> False

hgt :: Int -> String -> Bool
hgt n "cm" = n >= 150 && n <= 193
hgt n "in" = n >= 59 && n <= 76
hgt n (x:xs) | isDigit x = hgt (10 * n + ord x - ord '0') xs
             | otherwise = False
hgt _ _ = False

hcl :: String -> Bool
hcl ('#':hc) = length hc == 6 && all isHex hc
  where isHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
hcl _ = False

ecl :: String -> Bool
ecl ec = ec `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pid :: String -> Bool
pid p = length p == 9 && all isDigit p

isValidA :: Passport -> Bool
isValidA p = and $ hasField <$> ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] <*> pure p

isValidB :: Passport -> Bool
isValidB p = isValidA p && (and $ validations <*> pure p)
  where validations = [ validateField "byr" byr
                      , validateField "iyr" iyr
                      , validateField "eyr" eyr
                      , validateField "hgt" (hgt 0)
                      , validateField "hcl" hcl
                      , validateField "ecl" ecl
                      , validateField "pid" pid
                      ]

passports :: String -> [Passport]
passports = fmap (fmap read . words . unwords) . splitOn [""] . lines

instance Solved 4 'PartA where
  solve = show . length . filter isValidA . passports
instance Solved 4 'PartB where
  solve = show . length . filter isValidB . passports
