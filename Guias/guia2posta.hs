siguiente :: Integer -> Integer
siguiente = (1+)

mitad :: Double -> Double
mitad = (/2)

inversa :: Double -> Double
inversa = (1/)

triple :: Integer -> Integer
triple = (3*)

esNumeroPositivo :: Float-> Bool
esNumeroPositivo = (>= 0)

{-- esBisiesto :: Integral a => a -> Bool
esBisiesto x = esMultiploDe 400 x || esMultiploDe 4 x && not (esMultiploDe 100 x) --}

inversaRaizCuadrada :: Double -> Double
inversaRaizCuadrada = inversa.sqrt

cuadrado :: Float -> Float 
cuadrado = (**2)

suma :: Float -> Float -> Float
suma a b = a + b

incrementMCuadradoN :: Float -> Float -> Float
incrementMCuadradoN = suma.cuadrado

cuentaRara :: Num a => a -> a -> a -> a
cuentaRara c d e = ((c + 1) * d)+e

otraCuenta :: Integer -> Integer
otraCuenta = (*2)

composicionNueva :: Integer -> Integer -> Integer -> Integer
composicionNueva = cuentaRara.otraCuenta

potencia :: Int -> Int -> Int
potencia n m = n^m

esResultadoPar :: (Int -> Int) -> Int -> Bool
esResultadoPar f x = even (f x)
