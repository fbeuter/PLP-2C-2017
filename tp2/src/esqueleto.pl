%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(T, F, C, Cont) :- nth1(F, T, FilaT), nth1(C, FilaT, Cont).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T, F, C) :- contenido(T, F, C, Cont), var(Cont), forall(adyacenteEnRango(T,F,C,F1,C1), (contenido(T, F1, C1, Cont2), var(Cont2))).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(CantPiezas, horizontal, T, F, C) :- 
	piezasDisponibles(T, F, C, CantPiezas, CantPiezasm1), CM1 is C+1, puedoColocar(CantPiezasm1, horizontal, T, F, CM1).
puedoColocar(CantPiezas, vertical, T, F, C) :- 
	piezasDisponibles(T, F, C, CantPiezas, CantPiezasm1), FM1 is F+1, puedoColocar(CantPiezasm1, vertical, T, FM1, C).
puedoColocar(0, _, _, _, _).

piezasDisponibles(T, F, C, CantPiezas, CantPiezasm1) :- CantPiezas > 0, disponible(T, F, C), CantPiezasm1 is CantPiezas-1.

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarco(PiezasBarco, horizontal, T, F, C) :-
	oContenido(T, F, C, PiezasBarco, Pm1), CM1 is C+1, ubicarBarco(Pm1, horizontal, T, F, CM1).
ubicarBarco(PiezasBarco, vertical, T, F, C) :-
	oContenido(T, F, C, PiezasBarco, Pm1), FM1 is F+1, ubicarBarco(Pm1, vertical, T, FM1, C).
ubicarBarco(0, _, _, _, _).

oContenido(T, F, C, PiezasBarco, Pm1) :- contenido(T, F, C, o), Pm1 is PiezasBarco-1.

ubicarBarcos([X | Xs], T) :- X > 1, member(A,[vertical, horizontal]), puedoColocar(X, A, T, F, C), ubicarBarco(X, A, T, F, C), ubicarBarcos(Xs, T).
ubicarBarcos([1 | Xs], T) :- member(A,[vertical]), puedoColocar(1, A, T, F, C), ubicarBarco(1, A, T, F, C), ubicarBarcos(Xs, T).
ubicarBarcos([], _).

%convertirEnAguaOEsBarco(?Contenido)
convertirEnAguaOEsBarco(Contenido) :- term_to_atom(Contenido, ~).
convertirEnAguaOEsBarco(Contenido) :- Contenido == o.

%conAguaOBarco(?Fila)
conAguaOBarco(Fila) :- maplist(convertirEnAguaOEsBarco, Fila).

%completarConAgua(+?Tablero)
completarConAgua(Tablero) :- maplist(conAguaOBarco, Tablero).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear( [Fila|Tablero] , 1 , NumColumna , [FilaNueva|Tablero] ) :- reemplazarColumna(Fila,NumColumna,FilaNueva).
golpear( [Fila|Tablero] , NumFila , NumColumna , [Fila|TableroNuevo] ) :- NumFila > 1, FilaAnterior is NumFila-1, golpear( Tablero , FilaAnterior , NumColumna , TableroNuevo ).

reemplazarColumna( [_|Cs] , 1 , [~|Cs] ).
reemplazarColumna( [C|Cs] , NumColumna , [C|Res] ) :- NumColumna > 1, NumColumnaAnterior is NumColumna-1, reemplazarColumna( Cs , NumColumnaAnterior , Res ).

barcoUnaSolaPieza(T,F,C) :- forall(adyacenteEnRango(T,F,C,F2,C2), contenido(T,F2,C2,~)).

%Completar instanciación soportada y justificar.
%atacar(+Tablero,+NumFila,+NumColumna,-Resultado,-NuevoTab)
atacar(T, F, C, agua, T) :- contenido(T,F,C,~).
atacar(T, F, C, tocado, TNew) :- contenido(T,F,C,o), adyacenteEnRango(T,F,C,F2,C2), contenido(T,F2,C2,o), golpear(T,F,C,TNew).
atacar(T, F, C, hundido, TNew) :- contenido(T,F,C,o), barcoUnaSolaPieza(T,F,C), golpear(T,F,C,TNew).

% Ejercicio 8:
% Por la forma en la que definimos atacar el parametro Tablero es siempre indispensable 
% ya que por ejemplo es utilizado por contenido que lo requiere. Por otro lado los parametros NumFila y NumColumna
% son opcionales y esto es interesante ya que dado un tablero utilizando el predicado atacar se pueden generar
% todas las posibles jugadas validas y sus resultados (hundido, tocado o agua), mejor aun se puede instanciar resultado
% para buscar algún par de NumFila y NumColumna de interes.

%------------------Tests:------------------%

% Tests adyacenteEnRango
test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).

% Tests contenido
test(3) :- contenido([[0,1],[1,2],[3,4]], 1, 1, 0).
test(4) :- contenido([[0,1],[1,2],[3,4]], 3, 2, 4).

% Tests disponible
test(5) :- matriz(M,2,4), disponible(M,1,1).
test(6) :- not(disponible([[_, _, o],[_, _, _]], 2, 2)).
test(7) :- disponible([[_,_, _], [_,_, _], [_,_, _]], 2, 2).
test(8) :- disponible([[_,_, _], [_,_, _], [o,o, o]], 1, 1).
test(9) :- not(disponible([[o,_, _], [_,_, _], [o,o, o]], 1, 1)).

% Tests puedoColocar
test(10) :- matriz(M, 2, 2), puedoColocar(1, horizontal, M, 1, 1).
test(11) :- matriz(M, 2, 2), not( puedoColocar(3, horizontal, M, 1, 1) ).
test(12) :- not( puedoColocar(3, vertical, [[_,_,_], [_,o,_], [_,_,_]], 0, 0)).

% Tests ubicarBarco
test(13) :- matriz(M,3,3), ubicarBarco(2, horizontal, M, 1, 1), contenido(M, 1, 1, Y), Y==o.
test(14) :- matriz(M,3,3), contenido(M, 1, 2, o), ubicarBarco(2, horizontal, M, 1, 1), contenido(M, 1, 1, X), contenido(M, 1, 2, Y), X==o, Y==o.
test(15) :- matriz(M,1,1), not( ubicarBarco(2, horizontal, M, 1, 1) ).

% Tests completarConAgua
test(16) :- Tablero = [[o, o], [_, _], [_, o]], completarConAgua(Tablero), Tablero == [[o, o], [~, ~], [~, o]].

% Tests golpear
test(17) :- golpear([[o, o], [~, ~], [~, o]],1,2,[[o, ~], [~, ~], [~, o]]).
test(18) :- golpear([[o, o], [~, ~], [~, o]],2,2,[[o, o], [~, ~], [~, o]]).
test(19) :- golpear([[o, o], [~, ~], [~, o]],1,2,[[o, ~], [~, ~], [~, o]]).

% Tests atacar
test(20) :- atacar([[o, o], [~, ~], [~, o]],1,1,tocado,[[~, o], [~, ~], [~, o]]).
test(21) :- atacar([[o, o], [~, ~], [~, o]],3,1,agua,[[o, o], [~, ~], [~, o]]).
test(22) :- atacar([[o, o], [~, ~], [~, o]],3,2,hundido,[[o, o], [~, ~], [~, ~]]).

tests :- forall(between(1,22,N), test(N)).