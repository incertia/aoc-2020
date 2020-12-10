{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Problem (Part(..))
import Solver (solve)
import System.Environment (getProgName, getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  pname <- getProgName
  args <- getArgs
  let usage = putStrLn $ "usage: " ++ pname ++ " <problem-number> <A/B>"

  case args of
    [n, b] ->
      case readMaybe n :: Maybe Integer of
           Just n' ->
             case b of
                  "A" -> solve n' PartA
                  "B" -> solve n' PartB
                  _   -> usage
           Nothing -> usage
    _   -> usage
