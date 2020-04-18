module Bead where
import Data.Char
import Data.List

-----------------
--ascii dictionary
dictionary :: [String]
dictionary = [[chr c] | c <- [0..127]]

-----------------
--megadja, hogy a stringben a dictionary melyik es hanyadik eleme helyezkedik el a string elejen
prefixes :: String -> [String] -> [(Int,String)]
prefixes x (y:ys)
  |y `isPrefixOf` x = ( head $ (elemIndices y (y:ys), y ) : prefixes x ys
  |otherwise = []
