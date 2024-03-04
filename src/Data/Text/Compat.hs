{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module Data.Text.Compat (
    takeWord16
  , dropWord16
  , isASCII
  ) where

import qualified Data.Text        as T
import qualified Data.Text.Unsafe as UText
#if USE_TEXT_2
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
#endif

takeWord16 :: Int -> T.Text -> T.Text
takeWord16 n t =
#if USE_TEXT_2
  if n <= 0 then mempty else go n 0 0
  where
    go :: Int -> Int -> Int -> T.Text
    go 0 _ totalWord8 = UText.takeWord8 totalWord8 t
    go !leftToTake !iterIx !total = case UText.iter t iterIx of
      UText.Iter c offset
        | offset < 2
        -- utf8 case
        -> if isASCII c
              then go (leftToTake - 1) (iterIx + offset) (total + utf8Length c)
              else go leftToTake       (iterIx + offset) (total + utf8Length c)
        -- utf16 case
        | otherwise
        -> go (leftToTake - 1) (iterIx + offset) (total + utf8Length c)
#else
  UText.takeWord16 n t
#endif
{-# INLINE takeWord16 #-}

isASCII :: Char -> Bool
isASCII c = case fromEnum c of
  n | n <= 0x7F -> True  -- ASCII character, fits in one byte in UTF-8
  _             -> False

dropWord16 :: Int -> T.Text -> T.Text
dropWord16 n txt =
#if USE_TEXT_2
  if n <= 0 then txt else go n 0 txt
  where
    go :: Int -> Int -> T.Text -> T.Text
    go 0 _ t = t
    go !leftToDrop !iterIx t = case UText.iter t iterIx of
      UText.Iter c offset
        | offset < 2
        -- utf8 case
        -> if isASCII c
              then go (leftToDrop - 1) 0 (UText.dropWord8 (utf8Length c) t)
              else go leftToDrop  iterIx (UText.dropWord8 (utf8Length c) t)
        -- utf16 case
        | otherwise
        -> go (leftToDrop - 1) 0 (UText.dropWord8 (utf8Length c) t)

#else
  UText.dropWord16 n txt
#endif
{-# INLINE dropWord16 #-}
