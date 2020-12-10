{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Problem
  ( Part(..)
  , Solved(..)
  ) where

import Data.Hashable
  (Hashable)
import GHC.Generics
  (Generic)
import GHC.TypeNats
  (Nat)

data Part = PartA | PartB
  deriving (Eq, Show, Generic)

instance Hashable Part

class Solved (n :: Nat) (p :: Part) where
  solve :: String -> String
