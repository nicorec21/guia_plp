--ej2
valorAbsoluto :: Float -> Float
valorAbsoluto x 
  | x >= 0    = x
  | otherwise = -x

bisiesto :: Int -> Bool
bisiesto año
  | año `mod` 400 == 0 = True
  | año `mod` 100 == 0 = False
  | año `mod` 4 == 0   = True
  | otherwise          = False

factorial :: Int -> Int
factorial x
  | x == 0    = 1
  | otherwise = x * factorial (x - 1)

--ej3
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1 / x)

aEntero :: Either Int Bool -> Int
aEntero (Left x)  = x
aEntero (Right x) = if x then 1 else 0

--ej4
limpiar :: String -> String -> String
limpiar xs [] = [] 
limpiar xs (y:ys) | (y `elem` xs) = limpiar xs ys
                  | otherwise = y : limpiar xs ys

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio (x:xs) = (x - promedio(x:xs)) : difPromedio(xs)

promedio :: [Float] -> Float
promedio xs = sum xs / fromIntegral(length xs)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales[_] = True
todosIguales (x:y:xs) = ((x == y) && todosIguales(y:xs)) 

--ej5
data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAb Nil = True
vacio (Bin i r d) = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = r * (productoAB i) * (productoAB d)

