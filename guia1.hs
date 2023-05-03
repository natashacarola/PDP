-- Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3, p.ej: 
-- Main> esMultiploDeTres 9 
-- True

esMultiploDeTres :: Integral a => a -> Bool
esMultiploDeTres x = mod x 3 == 0

--Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero
esMultiploDe :: Integral a => a -> a -> Bool
esMultiploDe x y = mod y x == 0 

--Definir la función cubo/1, devuelve el cubo de un número
cubo :: Num a => a -> a
cubo x = x * x * x

--Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
area :: Num a => a -> a -> a
area base altura = base * altura

--Definir la función esBisiesto/1, indica si un año es bisiesto. 
--(Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) 
--Nota: Resolverlo reutilizando la función esMultiploDe/2

esBisiesto :: Integral a => a -> Bool
esBisiesto x = esMultiploDe 400 x || esMultiploDe 4 x && not (esMultiploDe 100 x)

--Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.
celsiusToFahr :: Fractional a => a -> a
celsiusToFahr temperaturaC = (temperaturaC * 9/5) + 32

--Definir la función fahrToCelsius/1, la inversa de la anterior.
fahrToCelsius :: Fractional a => a -> a
fahrToCelsius temperaturaFahr = (temperaturaFahr - 32) * 5/9

--Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. 
--Decimos que hace frío si la temperatura es menor a 8 grados Celsius

haceFrio :: (Ord a, Fractional a) => a -> Bool
haceFrio temperaturaFahr = fahrToCelsius temperaturaFahr < 8 

--Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, 
--de acuerdo a esta fórmula : mcm (a,b) = {a*b}/ {mcd (a,b)} 


mcm :: Integral a => a -> a -> a
mcm a b = (a*b) `div` gcd a b  


--Ej 10 dispersion 

dispersion :: (Num a, Ord a) => a -> a -> a -> a
dispersion x y z  = max x (max y z) - min x (min y z) 

diasParejos :: (Ord a, Num a) => a -> a -> a -> Bool
diasParejos x y z = dispersion x y z < 30

diasLocos :: (Ord a, Num a) => a -> a -> a -> Bool
diasLocos x y z = dispersion x y z > 100

diasNormales :: (Ord a, Num a) => a -> a -> a -> Bool
diasNormales x y z = not (diasParejos x y z || diasLocos x y z)

--Ej 11
pesoPino :: (Ord a, Num a) => a -> a
pesoPino altura 
            | altura > 300 = 2* altura
            |otherwise = 3* altura 

esPesoUtil :: (Ord a, Num a) => a -> Bool
esPesoUtil pesoEnKg
            | 400 < pesoEnKg && 1000> pesoEnKg = True
            |otherwise = False 
            

sirvePino :: (Ord a, Num a) => a -> Bool
sirvePino altura 
            | esPesoUtil (pesoPino altura) = True
            | otherwise = False
    

-- esCuadRadoPErfecto 30 false 
-- esCuadradoPerfecto 36 true --- esCuadradoPerfecto 25 -- esCuadradoPerfecto 16 -- esCuadradoPerfecto 9 -- esCuadradoPerfecto 4 -- esCuadrdoPerfecto 1 
-- (2*(6)-1)                                            --(2*(5)-1)
-- (2*sqrt(x)-1)
