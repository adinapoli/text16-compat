{-# LANGUAGE DeriveGeneric #-}
module Data.Text.Compat.Golden where

import Data.Text (Text)
import GHC.Generics

data GoldenTestOperation
  = GTO_takeWord16 !Int
  | GTO_dropWord16 !Int
  deriving (Show, Eq, Generic)

data GoldenOutput =
  GoldenOutput {
    testString    :: !Text
  , testOperation :: !GoldenTestOperation
  , testExpected  :: !Text
  } deriving (Show, Eq, Generic)
