module Xor where

import Data.Char

type BitString = [Int]
type Cipher = String
type Key = String

toBitString :: Int -> BitString
toBitString 0 = []
toBitString x
  |x `mod` 2 == 0 = [0] ++ toBitString (x `div` 2)
  |otherwise = [1] ++ toBitString (x `div` 2)

charToBitString :: Char -> BitString
charToBitString x = (toBitString $ ord x) ++ take (7 - (length $ toBitString $ ord x)) (repeat 0)

bitStringToChar :: BitString -> Char
bitStringToChar (x) = chr $ sum $ zipWith (*) (x) [2^y | y <- [0..length (x)]]

xor :: BitString -> BitString -> BitString
xor x y
  |length x == length y = map (`mod` 2) $ zipWith (+) x y
  |length x > length y = map (`mod` 2) $ zipWith (+) x (y ++ take (length x - length y) (repeat 0))
  |otherwise = map (`mod` 2) $ zipWith (+) y (x ++ take (length y - length x) (repeat 0))

--encrypt :: String -> Key -> Cipher

--decrypt :: encrypt

