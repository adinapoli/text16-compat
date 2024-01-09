{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy as BL
import Data.Text.Compat.Golden
import qualified Data.Vector as V
import System.IO
import Data.Csv
import qualified Data.Text as T
import qualified Data.Text.Compat as Compat
import qualified Data.Text.Encoding as TE

instance FromRecord GoldenOutput where
instance FromField GoldenTestOperation where
  parseField s = case T.breakOn ":" (TE.decodeUtf8 s) of
    ("dropWord16", n) -> pure $ GTO_dropWord16 (read $ T.unpack $ T.tail n)
    ("takeWord16", n) -> pure $ GTO_takeWord16 (read $ T.unpack $ T.tail n)
    (noOp, _n) -> fail $ "Unknown op: " <> T.unpack noOp

acquireGoldenRecords :: IO [GoldenOutput]
acquireGoldenRecords = do
  withFile "test-data/golden.csv" ReadMode $ \f -> do
    x <- BL.hGetContents f
    case decode NoHeader x of
      Left e  -> error e
      Right xs -> pure $ V.toList xs

main :: IO ()
main = do
  goldenTests <- acquireGoldenRecords
  defaultMain (tests goldenTests)

renderTestOperation :: GoldenTestOperation -> String
renderTestOperation = \case
  GTO_takeWord16 n -> "takeWord16 " <> show n
  GTO_dropWord16 n -> "dropWord16 " <> show n

goldenTestName :: GoldenOutput -> TestName
goldenTestName GoldenOutput{..} =
  renderTestOperation testOperation <> " " <> show (T.unpack testString )

tests :: [GoldenOutput] -> TestTree
tests goldenTests = testGroup "text16 Tests" $ map mkTest (zip [ 1 .. ] goldenTests)
  where
    mkTest :: (Int, GoldenOutput) -> TestTree
    mkTest (_ix, t@GoldenOutput{..}) = testCase ("[" <> show _ix <> "]" <> goldenTestName t) $
      case testOperation of
        GTO_takeWord16 n -> Compat.takeWord16 n testString @?= testExpected
        GTO_dropWord16 n -> Compat.dropWord16 n testString @?= testExpected
