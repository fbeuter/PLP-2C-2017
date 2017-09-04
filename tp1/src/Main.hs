import Diccionario
import Data.Maybe
import Arbol23
import Test.HUnit

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

--En este caso el iterate lo que hace es crear la lista infinita de pistas y sus resultados a partir de la pista inicial,
--a esta lista se la recorre con foldr. Se utilizo este tipo de fold ya que por la evaluacion lazy el mismo funcion sobre
--listas infinitas, la funcion del fold se encarga de chequear si es una pista o un tesoro, en caso de tener una pista no
--definida se retorna Nothing dando por finalizada la busqueda, si es un tesoro tambien se detiene la recursion y se retorna
--el valor, por ultimo, si es una pista valida y aun no se llego al tesoro, se continua la busqueda con la proxima pista.
listaDePistas::Eq a=>a->Diccionario a a->[Maybe a]
listaDePistas pista dict = iterate (\pista -> case pista of
                                                Nothing -> Nothing
                                                Just datoPista -> obtener datoPista dict)
                                   (Just pista)

busquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a 
busquedaDelTesoro pista esTesoro dict = foldr (\datoBusqueda rec -> case datoBusqueda of
                                                                        Nothing -> Nothing
                                                                        Just dato -> if (esTesoro dato) then
                                                                                            Just dato
                                                                                        else
                                                                                            rec)
                                              Nothing
                                              (listaDePistas pista dict)


{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

dicc4::Diccionario Int String
dicc4 = definirVarias [(1,"a"),(2,"b")] (vacio (<))

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  [True,False,True] ~=? internos arbolito2,
  "abcdefghi" ~=? hojas arbolito1,
  [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3),
  True ~=? esHoja (Hoja "a"),
  False ~=? esHoja arbolito1
  ]

testsEj3 = test [
  [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2),
  [True,False,True] ~=? internos (incrementarHojas arbolito2),
  [0,2,4,6,8,10,12,14] ~=? internos (duplicarElementos arbolito1),
  ["aa","bb","cc","dd","ee","ff","gg","hh","ii"] ~=? hojas (duplicarElementos arbolito1)
  ]

testsEj4 = test [
  [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3),
  6 ~=? altura (truncar 0 6 arbolito3),
  1 ~=? altura (truncar 0 1 arbolito3),
  [0] ~=? hojas (truncar 0 0 arbolito3),
  0 ~=? altura (truncar 0 0 arbolito3)
  ]

testsEj5 = test [
  -1 ~=? evaluar (truncar 0 3 arbolito3),
  1 ~=? evaluar (truncar 0 4 arbolito3),
  8 ~=? evaluar (truncar 0 5 arbolito3),
  22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

testsEj6 = test [
  0 ~=? 0
  ]

testsEj7 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj8 = test [
  (Nothing::Maybe Int) ~=? obtener (1::Int) (vacio (<)),
  (Just 42::Maybe Int) ~=? obtener 'a' (definir 'a' (42::Int) (vacio (<))),
  (Nothing::Maybe Int) ~=? obtener 'b' (definir 'a' (42::Int) (vacio (<))),
  (Just 43::Maybe Int) ~=? obtener 'b' (definir 'b' (43::Int) (definir 'a' (42::Int) (vacio (<))))
  ]


  
testsEj9 = test [
  [-10,0,2,9,15] ~=? claves dicc1,
  ["auto","calle","casa","escalera","inicio","ropero"] ~=? claves dicc2,
  [15,-10,0,2,9] ~=? claves dicc3,
  [1,2] ~=? claves dicc4
  ]


testsEj10 = test [
  Just "alfajor" ~=? busquedaDelTesoro "inicio" ((=='a').head) dicc2,
  Nothing ~=? busquedaDelTesoro "inicio" ((=='w').head) dicc2
  ]