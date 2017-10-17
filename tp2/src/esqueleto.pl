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
contenido(T, F, C, Cont) :- matriz(T, _, _), nth1(F, T, FilaT), nth1(C, FilaT, Cont).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T, F, C) :- contenido(T, F, C, Cont), var(Cont), forall(adyacenteEnRango(T,F,C,F1,C1), (contenido(T, F1, C1, Cont2), var(Cont2))).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(CantPiezas, horizontal, T, F, C) :- 
	CantPiezas > 0, disponible(T, F, C), CantPiezasm1 is CantPiezas-1, CM1 is C+1, puedoColocar(CantPiezasm1, horizontal, T, F, CM1).
puedoColocar(CantPiezas, vertical, T, F, C) :- 
	CantPiezas > 0, disponible(T, F, C), CantPiezasm1 is CantPiezas-1, FM1 is F+1, puedoColocar(CantPiezasm1, vertical, T, FM1, C).
puedoColocar(0, _, _, _, _).

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarco(PiezasBarco, horizontal, T, F, C) :-
	contenido(T, F, C, o), CM1 is C+1, Pm1 is PiezasBarco-1, ubicarBarco(Pm1, horizontal, T, F, CM1).
ubicarBarco(PiezasBarco, vertical, T, F, C) :-
	contenido(T, F, C, o), FM1 is F+1, Pm1 is PiezasBarco-1, ubicarBarco(Pm1, vertical, T, FM1, C).
ubicarBarco(0, _, _, _, _).

ubicarBarcos([X | Xs], T) :- X \= 1, matriz(T, _, _), puedoColocar(X, horizontal, T, F, C), ubicarBarco(X, horizontal, T, F, C), ubicarBarcos(Xs, T).
ubicarBarcos([X | Xs], T) :- X \= 1, matriz(T, _, _), puedoColocar(X, vertical, T, F, C), ubicarBarco(X, vertical, T, F, C), ubicarBarcos(Xs, T).
ubicarBarcos([1 | Xs], T) :- matriz(T, _, _), puedoColocar(1, horizontal, T, F, C), ubicarBarco(1, horizontal, T, F, C), ubicarBarcos(Xs, T).
ubicarBarcos([], _).

%convertirEnAguaOEsBarco(?Contenido)
convertirEnAguaOEsBarco(Contenido) :- term_to_atom(Contenido, ~).
convertirEnAguaOEsBarco(Contenido) :- Contenido == o.

%conAguaOBarco(?Fila)
conAguaOBarco(Fila) :- maplist(convertirEnAguaOEsBarco, Fila).

%completarConAgua(+?Tablero)
completarConAgua(Tablero) :- matriz(Tablero,_,_), maplist(conAguaOBarco, Tablero).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(T,N,M,TNew) :- matriz(T,Filas,Columnas), enRango(T,N,M), matriz(TNew,Filas,Columnas), 
	forall(
		between(1,Filas,I), (
			forall(
				between(1,Columnas,J), (
						((I \= N; J \= M), contenido(T, I, J, Cont), contenido(TNew, I, J, Cont));
						(I == N, J == M, contenido(TNew, I, J, ~))
				)
			) 
		)
	).

barcoUnaSolaPieza(T,F,C) :- forall(adyacenteEnRango(T,F,C,F2,C2), contenido(T,F2,C2,~)).

%Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)
atacar(T, F, C, agua, T) :- contenido(T,F,C,~).
atacar(T, F, C, tocado, TNew) :- contenido(T,F,C,o), adyacenteEnRango(T,F,C,F2,C2), contenido(T,F2,C2,o), golpear(T,F,C,TNew).
atacar(T, F, C, hundido, TNew) :- contenido(T,F,C,o), barcoUnaSolaPieza(T,F,C), golpear(T,F,C,TNew).

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
tests :- forall(between(1,2,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.