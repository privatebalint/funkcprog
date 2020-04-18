module Morze where

type Bit = Int
type BitString = [Bit]
type Byte = [Bit]

intToBitString :: Int -> BitString
intToBitString 0 = [0]
intToBitString x
  |x<256 && x>1 && x `mod` 2 == 1 = intToBitString (x `div` 2) ++ [1]
  |x<256 && x>1 && x `mod` 2 == 0 = intToBitString (x `div` 2) ++ [0]
  |x<256 && x==1 = [1]

intToByte :: Int -> Byte
intToByte x
  |(length $ intToBitString x) < 8 = take (8 - (length $ intToBitString x)) (repeat 0) ++ intToBitString x
  |otherwise = intToBitString x

parity :: BitString -> Bit
parity x
  |even $ sum x = 0
  |otherwise = 1

insertElemAt :: a -> [Int] -> [a] -> [a]
insertElemAt x [y] z = take (y-1) z ++ x : drop (y-1) z
insertElemAt x (y:ys) z = insertElemAt x ys $ insertElemAt x [y] z

