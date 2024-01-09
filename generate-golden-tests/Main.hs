{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
module Main where

import Control.Monad
import Data.Csv as CSV
import Data.Csv.Builder (encodeRecordWith)
import Data.Text.Arbitrary ()
import Data.Text.Compat as Compat
import Data.Text.Compat.Golden
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Builder as BLD
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

genOperation :: Int -> Gen GoldenTestOperation
genOperation inputStringSize = do
  n  <- frequency [(100, choose (0, inputStringSize - 1)), (100, choose (-2, inputStringSize + 3))]
  op <- elements [GTO_dropWord16 n, GTO_takeWord16 n]
  pure op

instance ToField GoldenTestOperation where
  toField = \case
    GTO_dropWord16 n -> "dropWord16:" <> C8.pack (show n)
    GTO_takeWord16 n -> "takeWord16:" <> C8.pack (show n)

instance ToRecord GoldenOutput where

instance Arbitrary GoldenOutput where
  arbitrary = do
    txt <- T.filter (\c -> c /= ',' && c /= '\n' && c /= '\r' && c /= '\t') <$> arbitrary
    op  <- genOperation (T.length txt)
    pure $ GoldenOutput txt op mempty

main :: IO ()
main = do
  !tests <- map injectTest <$> generate (vectorOf 1_000_000 arbitrary)
  let sz = length tests
  forM_ (zip [1 .. ] tests) $ \(_ix, testEntry) -> do
    let encoded = encodeRecordWith (CSV.defaultEncodeOptions { encUseCrLf = False }) testEntry
    putStr $ "Encoding record [" <> show _ix <> "/" <> show sz <> "]: " <> (show $ C8.unpack $ BL.toStrict $ BLD.toLazyByteString $ encoded)
    B.appendFile "test-data/golden.csv" $ BL.toStrict $ BLD.toLazyByteString encoded
    putStrLn $ " ... done."
  where
    injectTest :: GoldenOutput -> GoldenOutput
    injectTest o@GoldenOutput{..} =
      case testOperation of
        GTO_dropWord16 n -> o { testExpected = Compat.dropWord16 n testString }
        GTO_takeWord16 n -> o { testExpected = Compat.takeWord16 n testString }
