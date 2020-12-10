{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module AOC008 () where

import Console
  (Instruction(..), Console, Error(..), ConsoleHook, toConsole, acc, runWithHook, throw)
import Control.Lens
  ((.=), view, use)
import Control.Monad
  (when)
import Control.Monad.Except
  (catchError, runExceptT)
import Control.Monad.State
  (get, put, execState)
import Data.Has
  (Has(..))
import Data.HashSet
  (HashSet, member)
import Problem
  (Solved(..), Part(..))

repeatHook :: Has (HashSet Integer) s => ConsoleHook s m
repeatHook cpc i continue = do
  pcs <- use hasLens
  when (cpc `member` pcs) $ throw Reexecuted
  hasLens .= [cpc] <> pcs
  continue i

flipHook :: (Has (HashSet (Integer, Bool)) s, Has Bool s) => ConsoleHook s m
flipHook cpc inst continue = do
  pcs <- use hasLens
  modified <- use hasLens
  when ((cpc, modified) `member` pcs) $ throw Reexecuted
  hasLens .= [(cpc, modified)] <> pcs

  if not modified && modifiable inst then do
    og <- get
    hasLens .= [(cpc, True)] <> pcs
    hasLens .= True
    -- try to run the new program
    -- if it fails, restore the state
    continue (modify inst) `catchError` \case
      Reexecuted -> do
        npcs <- use $ hasLens @(HashSet (Integer, Bool))
        put og
        hasLens .= npcs
        continue inst
      e -> throw e
  else
    continue inst
  where modify (Jmp i) = Nop i
        modify (Nop i) = Jmp i
        modify i = i
        modifiable (Jmp _) = True
        modifiable (Nop _) = True
        modifiable _ = False

checkLoop :: Console -> (Console, HashSet Integer)
checkLoop = execState (runExceptT $ runWithHook repeatHook) . (,mempty)

checkFlip :: Console -> (Console, HashSet (Integer, Bool), Bool)
checkFlip = execState (runExceptT $ runWithHook flipHook) . (,mempty,False)

solve8 :: Has Console s => (Console -> s) -> String -> String
solve8 check = show . view (hasLens . acc) . check . toConsole . fmap read . lines

instance Solved 8 'PartA where
  solve = solve8 checkLoop
instance Solved 8 'PartB where
  solve = solve8 checkFlip
