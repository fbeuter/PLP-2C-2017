module Arbol23 where

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength = 5    
    
padTree:: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b)-> String
padTree nivel acum doPad t = case t of 
				  (Hoja x) -> initialPad ++ stuff x
                                  (Dos x i d) -> initialPad ++ stuff x ++ 
                                                 pad padlength ++ rec x False i ++ "\n" ++
                                                 rec x True d ++ "\n"
                                  (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                                                      pad padlength ++ rec x False i ++ "\n" ++
                                                      pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                                                      rec x True d ++ "\n" 
  where l = length . stuff
	levelPad = (padlength*nivel + acum)
	initialPad = (if doPad then pad levelPad else "")
	rec x = padTree (nivel+1) (acum+l x)
            
stuff:: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

foldA23:: (b -> b -> c -> c -> c -> c) -> (b -> c -> c -> c) -> (a -> c) -> Arbol23 a b -> c
foldA23 f g h (Hoja dato) = h dato
foldA23 f g h (Dos dato arbol1 arbol2) = g dato (foldA23 f g h arbol1) (foldA23 f g h arbol2)
foldA23 f g h (Tres dato1 dato2 arbol1 arbol2 arbol3) = f dato1 dato2 (foldA23 f g h arbol1) (foldA23 f g h arbol2) (foldA23 f g h arbol3)

--Lista en preorden de los internos del árbol.
internos::Arbol23 a b->[b]
internos = foldA23 (\d1 d2 r1 r2 r3 -> [d1] ++ [d2]++ r1 ++ r2 ++ r3)
                   (\d r1 r2 -> [d] ++ r1 ++ r2)
                   (\_ -> [])

--Lista las hojas de izquierda a derecha.
hojas::Arbol23 a b->[a]
hojas = foldA23 (\d1 d2 r1 r2 r3 -> r1 ++ r2 ++ r3)
                (\d r1 r2 -> r1 ++ r2)
                (\d -> [d])

esHoja::Arbol23 a b->Bool
esHoja x = case x of
                    (Hoja a) -> True
                    _        -> False

mapA23::(a->c)->(b->d)->Arbol23 a b->Arbol23 c d
mapA23 f g = foldA23  (\d1 d2 r1 r2 r3 -> Tres (g d1) (g d2) r1 r2 r3)
                      (\d r1 r2 -> Dos (g d) r1 r2)
                      (\d -> Hoja (f d))

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id


--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f g 0 = g
foldNat f g n = f n (foldNat f g (n-1))

-- potencia a b = foldNat (\_ rec -> a * rec) 1 b
-- la hize con una recursion "no explicita" medio turbia, no se cuan correcto es esto, pero no entendi la sugerencia del enunciado
truncar::a->Integer->Arbol23 a b->Arbol23 a b
truncar h n arb = if n==0 then (Hoja h)
                  else case arb of
                    (Tres d1 d2 a1 a2 a3) -> (Tres d1 d2 (truncar h (n-1) a1)
                                                         (truncar h (n-1) a2)
                                                         (truncar h (n-1) a3))
                    (Dos d a1 a2)         -> (Dos d (truncar h (n-1) a1)
                                                    (truncar h (n-1) a2))
                    (Hoja d)              -> (Hoja d)

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar::Arbol23 a (a->a->a)->a
evaluar = foldA23 (\op1 op2 val1 val2 val3 -> op2 (op1 val1 val2) val3)
                  (\op val1 val2 -> op val1 val2)
                  (\val -> val)

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}
arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4::Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12))) 
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))
