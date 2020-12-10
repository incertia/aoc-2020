{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Console
  ( Instruction(..)
  , ConsoleState(..)
  , _Stopped
  , _Running
  , _AwaitingInput
  , _Halted
  , _Terminated
  , _Threw
  , Console
  , acc
  , pc
  , prog
  , Program
  , Error(..)
  , ConsoleHook
  , toConsole
  , toProgram
  , runWithHook
  , run
  , throw
  ) where

import Control.Lens
  ((.=), (+=), use, preuse, ix)
import Control.Lens.TH
  (makeLenses, makePrisms)
import Data.Hashable
  (Hashable)
import Control.Monad
  (when)
import Control.Monad.Except
  (MonadError, throwError)
import Control.Monad.State
  (MonadState)
import Data.Char
  (isDigit)
import Data.Has
  (Has(..))
import Data.HashMap.Strict
  (HashMap, fromList)
import GHC.Generics
  (Generic)
import Text.ParserCombinators.ReadP
  (choice, string, char, munch1)
import Text.ParserCombinators.ReadPrec
  (lift)
import Text.Read
  (Read(..))

data Instruction = Acc Integer
                 | Jmp Integer
                 | Nop Integer
                 | Hlt
  deriving (Show, Eq, Generic)
instance Hashable Instruction

type Program = HashMap Integer Instruction

data ConsoleState = Stopped
                  | Running
                  | AwaitingInput
                  | Halted
                  | Terminated
                  | Threw
  deriving (Eq, Show, Enum, Bounded)
makePrisms ''ConsoleState

data Console =
  Console
  { _acc    :: Integer
  , _pc     :: Integer
  , _prog   :: Program
  , _state  :: ConsoleState
  }
  deriving (Eq, Show)
makeLenses ''Console

data Error = NoInst
           | Reexecuted
  deriving (Show, Eq, Enum, Bounded)

type ConsoleHook s m = (Has Console s, MonadState s m, MonadError Error m)
                     => (Integer -> Instruction -> (Instruction -> m ()) -> m ())

instance Read Instruction where
  readPrec = lift $ do
    ins <- choice [string "acc" >> pure Acc, string "jmp" >> pure Jmp, string "nop" >> pure Nop]
    _ <- char ' '
    sgn <- choice [char '+' >> pure id, char '-' >> pure negate]
    arg <- read <$> munch1 isDigit
    return $ ins (sgn arg)

toConsole :: [Instruction] -> Console
toConsole is = Console 0 0 (toProgram is) Stopped

toProgram :: [Instruction] -> Program
toProgram = fromList . zip [0..] . (<>[Hlt])

runWithHook :: (Has Console s, MonadState s m, MonadError Error m)
            => ConsoleHook s m -> m ()
runWithHook hook = do
  hasLens . state .= Running
  cpc <- use $ hasLens . pc
  mins <- preuse $ hasLens . prog . ix cpc
  case mins of
    Just ins -> hook cpc ins (step hook)
    Nothing -> throw NoInst

step :: (Has Console s, MonadState s m, MonadError Error m)
     => ConsoleHook s m -> Instruction -> m ()
step hook ins = do
  -- change states
  case ins of
    Hlt -> hasLens . state .= Halted
    _   -> pure ()

  -- only execute if we are still running, otherwise just return
  st <- use $ hasLens . state
  when (st == Running) $ do
    case ins of
      Acc i -> hasLens . acc += i >> hasLens . pc += 1
      Jmp o -> hasLens . pc += o
      Nop _ -> hasLens . pc += 1
      Hlt   -> pure ()
    runWithHook hook

run :: (Has Console s, MonadState s m, MonadError Error m) => m ()
run = runWithHook $ \_ i s -> s i

throw :: (Has Console s, MonadState s m, MonadError Error m) => Error -> m ()
throw e = hasLens . state .= Threw >> throwError e
