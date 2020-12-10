{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module AOC007 () where

import Control.Lens
  (ASetter', over, (^?), (?~))
import Control.Lens.At
  (at, ix)
import Data.Char
  (isDigit)
import Data.Foldable
  (foldl', toList)
import Data.Function
  ((&))
import Data.List
  (intercalate)
import Data.List.Split
  (split, splitOn, whenElt)
import Data.HashMap.Strict
  (HashMap)
import Data.HashSet
  (HashSet, singleton, member)
import Problem
  (Solved(..), Part(..))

import qualified Data.HashSet as HS
  (map)

data Relation = Relation String (HashSet (Int, String))
  deriving (Show, Eq)

type UpGraph = HashMap String (HashSet String)
type DownGraph = HashMap String (HashSet (Int, String))

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace n r h = intercalate r (splitOn n h)

makeRelation :: [String] -> Relation
makeRelation (x:xs) = Relation x (relations xs)
  where relations (a:b:rest) = singleton (read a, b) <> relations rest
        relations [] = mempty
        relations _ = error "impossibru"
makeRelation _ = error "impossible"

buildContainer :: UpGraph -> Relation -> UpGraph
buildContainer g (Relation up rs) = foldl' (\g' r -> g' & at (snd r) <>? singleton up) g rs

buildContained :: DownGraph -> Relation -> DownGraph
buildContained g (Relation up rs) = g & at up ?~ rs

(<>?) :: Monoid a => ASetter' s (Maybe a) -> a -> s -> s
l <>? a = over l (fmap (<>a) . maybe (Just mempty) Just)

bfsA :: String -> HashSet String -> UpGraph -> HashSet String
bfsA start visited g =
  if start `member` visited then mempty
  else
    case g ^? ix start of
      Just ups -> ups <> foldl' (<>) mempty (HS.map (\u -> bfsA u visited' g) ups)
      Nothing  -> singleton start
  where visited' = visited <> singleton start

bfsB :: String -> HashSet String -> DownGraph -> Int
bfsB start visited g =
  if start `member` visited then 0
  else
    case g ^? ix start of
      Just rs -> sum ((\(n, r) -> n + n * bfsB r visited' g) <$> toList rs)
      Nothing -> 0
  where visited' = visited <> singleton start

solver :: Part -> String -> String
solver p i = show ans
  where relations = fmap makeRelation . cleanup' . cleanup $ i
        cleanup = replace "," "" . replace "." "" . replace "bag" "" . replace "bags" "bag" . replace "contain" ""
        cleanup' = fmap (fmap concat . split (whenElt (all isDigit)) . words) . lines
        container = foldl' buildContainer mempty $ relations
        contained = foldl' buildContained mempty $ relations
        ans = case p of { PartA -> ansA; PartB -> ansB; }
        ansA = length $ bfsA "shinygold" mempty container
        ansB = bfsB "shinygold" mempty contained

instance Solved 7 'PartA where
  solve = solver PartA
instance Solved 7 'PartB where
  solve = solver PartB
