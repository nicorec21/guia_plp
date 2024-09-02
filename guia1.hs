--ej1
max2 :: (Int, Int) -> Int --No curry
max2 (x, y) | x >= y = x
    | otherwise = y

normaVectorial :: (Float, Float) -> Float --No curry
normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract2 :: Float -> Float -> Float --Curry
subtract2 = flip (-)  

predecesor :: Float -> Float --Curry
predecesor = subtract 1  

evaluarEnCero :: (Float -> Float) -> Float --Curry
evaluarEnCero = \f -> f 0

dosVeces :: (Float -> Float) -> (Float -> Float)
dosVeces = \f -> f . f

flipAll :: [(Float -> Float -> Float)] -> [(Float -> Float -> Float)]
flipAll = map flip

--Si flip :: (a->b->c) -> b -> a -> c  entonces flip flip :: b -> (a->b->c) -> a -> c 
flipRaro = flip flip

--ej2
curry2 :: ((a,b) -> c) -> (a -> b -> c)
curry2 f x y = f(x,y)

--f para probar curry: curry2 addTuple 2 2
addTuple :: (Int, Int) -> Int
addTuple (x, y) = x + y

uncurry2 :: (a -> b -> c) -> ((a,b) -> c)
uncurry2 f(x,y) = f x y

--f para probar uncurry: uncurry2 add2 (2,2)
add2 :: Int -> Int -> Int
add2 x y = x + y

--curryN :: curryN :: ((a, b, ..., z) -> r) -> (a -> b -> ... -> z -> r)

--foldr:: (a -> b -> b) -> b -> [a] -> b
--ej3
sumConFoldr :: [Float] -> Float
sumConFoldr = foldr (+) 0 

concatConFoldr :: [a] -> [a] -> [a]
concatConFoldr xs ys = foldr (:) ys xs

elemConFoldr ::  Eq a => a -> [a] -> Bool 
elemConFoldr y = foldr (\x acc -> (x == y) || acc) False 

filterConFoldr :: (a -> Bool) -> [a] -> [a]
filterConFoldr f = foldr (\x rec -> if f x then x : rec else rec) []

mapConFoldr :: (a -> a) -> [a] -> [a]
mapConFoldr f = foldr(\x rec -> f x : rec) []

mejorSegun ::  (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

sumasParciales ::  Num a => [a] -> [a]
sumasParciales = foldl(\rec x -> rec ++ (if null rec then [x] else [x + last rec])) []

sumaAlt :: Num a => [a] -> a 
sumaAlt = foldr1 (-)

sumaAlt2 :: Num a => [a] -> a 
sumaAlt2 = foldl1 (flip (-)) 

--ej5
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)
--Rec global, no se puede reescribir (facilmente) con fold
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

entrelazarConFoldr :: [a] -> [a] -> [a] 
entrelazarConFoldr = foldr f (const [])
    where f = \x acc yys -> if null yys then x : acc [] else x : head yys : acc (tail yys)