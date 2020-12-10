{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module AOC010 () where

import Control.Arrow
  ((&&&))
import Control.Lens
  (ix, (%~))
import Data.Function
  ((&))
import Data.List
  (sort, maximumBy, groupBy)
import Data.Vector
  (Vector, fromList, (!))
import Problem
  (Solved(..), Part(..))

import qualified Data.Vector as V
  (head, tail, cons, filter, length)

jd :: Integer -> Vector Integer -> Integer
jd n v =
  if V.length v >= 2 then
    (if v ! 1 - v ! 0 == n then 1 else 0) + jd n (V.tail v)
  else 0

jolts :: String -> Vector Integer
jolts inp = let x = fmap read . lines $ inp in fromList . sort $ 0 : maximum x + 3 : x

groupjolts :: Vector Integer -> Vector (Vector Integer)
groupjolts = V.filter (not . null)
           . fmap ((\v -> let m = V.head v in (`subtract` m) <$> V.tail v) . fmap fst)
           . vgroup snd . findgroups 0
  where findgroups i v =
          if V.length v >= 2 then
            let (x, y) = (v ! 0, v ! 1)
                t = V.tail v
                rest = if y - x == 3 then V.cons (y, i) (findgroups (i + 1) t)
                       else findgroups i t
            in  V.cons (x, i) rest
          else (,i) <$> v
        vgroup f v =
          if null v then []
          else if V.length v == 1 then [v]
          else
            let r = vgroup f v
            in  if f (r ! 0 ! 0) == f (v ! 0) then
                   r & ix 0 %~ V.cons (v ! 0)
                else V.cons [v ! 0] r

ways :: Vector Integer -> Integer
ways v = go 0 v where
  go j v =
    if null v then if j == m then 1 else 0
    else if v ! 0 > j + 3 then 0
    else go (v ! 0) (V.tail v) + go j (V.tail v)
  m = maximum v

instance Solved 10 'PartA where
  solve = show . uncurry (*) . (jd 1 &&& jd 3) . jolts
instance Solved 10 'PartB where
  solve = show . product . fmap ways . groupjolts . jolts
