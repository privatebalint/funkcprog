module Hamming where

import Data.List

morseTab :: [(Char, String)]
morseTab =
  [('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),('E',".")
  ,('F',"..-."),('G',"--."),('H',"...."),('I',".."),('J',".---")
  ,('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),('O',"---")
  ,('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),('T',"-")
  ,('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-")
  ,('Y',"-.--"),('Z',"--..")
  ,('0',"-----"),('1',".----"),('2',"..---"),('3',"...--"),('4',"....-")
  ,('5',"....."),('6',"-...."),('7',"--..."),('8',"---.."),('9',"----.")]

----A kodolando szoveg normalizalasa (done) 1p
normalizeText :: String -> String
normalizeText [] = []
normalizeText (x:xs)
  |elem x (fst $ unzip morseTab) = x : (normalizeText xs)
  |otherwise = normalizeText xs

----Kodolas (done) 1p
charToCode :: [(Char,String)] -> Char -> String
charToCode ((x,y):z) xs
  |xs == x = y
  |otherwise = charToCode z xs

----Szoveg kodszavakka kodolasa (done) 1p
encodeToWords :: String -> [String]
encodeToWords [] = []
encodeToWords (x:xs) = (charToCode morseTab x : encodeToWords xs)


----Kodolas szovegge (done) 1p
encodeString :: String -> String
encodeString x = intercalate "" (encodeToWords x)

----Visszakodolas (done) 1p
codeToChar :: [(a,String)] -> String -> a
codeToChar ((x,y):z) p
  |p == y = x
  |otherwise = codeToChar z p

----Kodszavak visszakodolasa (done) 1p
decodeWords :: [String] -> String
decodeWords [] = []
decodeWords (x:xs) = (codeToChar morseTab x : decodeWords xs)

----Legrovidebb kod (not done) 3p

----A szoveg elejere illeszkedo kodok megtalalasa (not done) 2p

----A lehetseges ertelmezesek eloallitasa (not done) 5p

---Elert pontszam: 6
