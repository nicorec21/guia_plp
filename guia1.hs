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

--ej3
