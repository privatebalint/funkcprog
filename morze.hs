module Morze where

type Bit = Int
type BitString = [Bit]
type Byte = [Bit]

----Szam bitsorozatta konvertalasa (done) 1p
intToBitString :: Int -> BitString
intToBitString 0 = [0]
intToBitString x
  |x<256 && x>1 && x `mod` 2 == 1 = intToBitString (x `div` 2) ++ [1]
  |x<256 && x>1 && x `mod` 2 == 0 = intToBitString (x `div` 2) ++ [0]
  |x<256 && x==1 = [1]

----Bitsorozat -> Byte (done) 1p
intToByte :: Int -> Byte
intToByte x
  |(length $ intToBitString x) < 8 = take (8 - (length $ intToBitString x)) (repeat 0) ++ intToBitString x
  |otherwise = intToBitString x

----Elem beszurasa adott poziciokon (done) 1p
insertElemAt :: a -> [Int] -> [a] -> [a]
insertElemAt x [y] z = take (y-1) z ++ x : drop (y-1) z
insertElemAt x (y:ys) z = insertElemAt x ys $ insertElemAt x [y] z

----Sorozatok reszei (not done) 2p


----Bitsorozat paritasa (done) 1p
parity :: BitString -> Bit
parity x
  |even $ sum x = 0
  |otherwise = 1

----Elemek beszurasa adott poziciokon (done) 1p
insertElemsAt :: [(a,Int)] -> [a] -> [a]
insertElemsAt [x] y = take (snd x-1) y ++ fst x : drop (snd x-1) y
insertElemsAt (x:xs) y = insertElemsAt xs $ insertElemsAt [x] y

----Haming eloallitasa (not done) 3p



----Hamming kod validalasa (done) 1p
validate :: BitString -> Bool
validate x = ((parity $ p 1 x) == 0) && ((parity $ p 2 x) == 0) && ((parity $ p 4 x) == 0) && ((parity $ p 8 x) == 0)

----Hiba megkeresese (not done) 1p



----Hiba javitasa (done) 2p
correctError x
  |validate x = x
  |validate x == False && (length x > findErrorPosition x) = (take (findErrorPosition x -1) x) ++ (abs ((last $ fst $ splitAt (findErrorPosition x) x) -1)) : drop (findErrorPosition x) x

---eddig osszesen: 8p
---megoldott feladatok: 7
---megmaradt feladatok: 3
