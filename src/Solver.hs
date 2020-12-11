{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Solver (Solver.solve) where

import Control.Exception (try)
import Control.Lens ((^?), ix)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Generator (generateMap)
import Problem (Solved(..), Part)

import AOC001 ()
import AOC002 ()
import AOC003 ()
import AOC004 ()
import AOC005 ()
import AOC006 ()
import AOC007 ()
import AOC008 ()
import AOC009 ()
import AOC010 ()
import AOC011 ()

generateMap ''Solved 'Problem.solve "solvers"

timed :: IO () -> IO ()
timed io = do
  start <- getPOSIXTime
  io
  end <- getPOSIXTime
  putStrLn $ "solution took " ++ show (round @_ @Integer ((end - start) * 1000)) ++ "ms"

solve :: Integer -> Part -> IO ()
solve n b = case solvers ^? ix (n, b) of
  Nothing -> putStrLn "no solver implemented"
  Just f  -> do
    let ifile = "./input/" ++ pad 3 '0' (show n)
    inp <- either (const "") id <$> try @IOError (readFile ifile)
    timed $ putStrLn (f inp)
  where pad l x xs = if length xs < l then pad l x (x:xs) else xs
