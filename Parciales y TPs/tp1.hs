esVocal :: Char -> Bool
esVocal 'a' = True 
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False

estaEntre :: Int -> Int -> Int-> Bool 
estaEntre x y z = (z<y && z>x) || (z>y && z<x) 

potencia :: Float -> Int -> Float
potencia _ 0 = 1.0
potencia 0 _ = 0
potencia x y
    | y > 0     = x * potencia x (y-1)
    | otherwise = 1.0 / potencia x (-y)