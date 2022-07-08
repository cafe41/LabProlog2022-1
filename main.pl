% PARADIGMAS DE LA PROGRAMACIÓN
% SECCIÓN DEL CURSO: 0-B-2
% PROFESOR DE TEORÍA: VICTOR FLORES
% PROFESOR DE LABORATORIO: VICTOR FLORES
%
% AUTOR
% NOMBRE: Gustavo Andrés Vergara Parra
% RUT: 19.810.849-9
% CARRERA: Ingenieria en Ejecucion en Informatica
% VERSIÓN PROLOG: 8.4.2

% Copypasteamos para ver la lista completa:
% set_prolog_flag(answer_write_options,[max_depth(0)]), true.

%Base de datos

%HECHOS

%REGLAS

%addElemento1
%Dominio: int X list X variable
%Recorrido: list
%Recursión: Natural
%Resumen: "append" casero, usado en crearCarta1
addElemento1(Elemento, [], [Elemento]):- !.
addElemento1(Elemento, [C|R], [C|L]):-
    addElemento1( Elemento, R, L). % Donde R es el resto y L es la lista.
%Ejemplo de uso: addElemento1(3, [1, 2, 4], X).

%calcularOrden(numE, Orden).
%Dominio: int X variable
%Ejemplo de uso: calcularOrden(7, O).
%Recorrido: int
%Recursión: No hay
%Resumen: Hecho que calcula el Orden de un cardsSet
calcular_orden(NumE, Orden):-
    Orden is NumE-1.
%Ejemplo de uso: calcular_orden(4, Orden),

%crearCarta1
%Dominio: list X int X list X variable
%Recorrido: list
%Recursión: de Cola
%Hecho que crea la primera carta de un CardsSet.
crearCarta1(_, 0, R, R):- !. %Caso Base
crearCarta1([CAR|CDR], NumE, Carta, R):- %Caso Recursivo
    addElemento1(CAR, Carta, X),
    Nmenos1 is (NumE -1),
    crearCarta1(CDR, Nmenos1, X, R).
%Ejemplo de uso: crearCarta1([1,2,3], 3, [],X).

%crearCartaN
%DOM: list X int X int X int X list X variable
%REC: list
%Recursión: De cola
%Hecho que crea una "cartaN" de un CardsSet.
%Caso Base
crearCartaN([CAR|CDR], N, J, N, Carta, R):- 
    Num is ((N * J) + (N+1)), 
    nth1(Num, [CAR|CDR], X),
    append(Carta, [X], CartaN),
    R = CartaN, !.
%Caso "Vacío"
crearCartaN([CAR|CDR], N, J, K, [], R):-
    crearCartaN([CAR|CDR], N, J, K, [CAR], R).
%Caso Recursivo
crearCartaN([CAR|CDR], N, J, K, Carta, R):-
    Num is ((N * J) + (K+1)), 
    nth1(Num, [CAR|CDR], X),
    append(Carta, [X], CartaN),
    K1 is (K+1),
    crearCartaN([CAR|CDR], N, J, K1, CartaN, R).
%Ejemplo de uso: crearCartaN([1,2,3,4,5,6,7], 3, 1, 1, [1], X).

%cicloCrearCartasN
%DOM: list X int X int X int X list X variable
%REC: list
%Recursión: de cola
%%Resumen: Ciclo que crea todas las "cartasN" de un cardsSet
%Caso Base
cicloCrearCartasN(Elementos, N, N, K, CardsSet, R):- 
    crearCartaN(Elementos, N, N, K, [], X),
    append(CardsSet, [X], Y),
    R = Y,!.
%Caso Recursivo
cicloCrearCartasN(Elementos, N, J, K, CardsSet, R):-
    crearCartaN(Elementos, N, J, K, [], X),
    append(CardsSet, [X], Y),
    J1 is (J+1),
    cicloCrearCartasN(Elementos, N, J1, K, Y, R).
%Ejemplo de uso: cicloCrearCartasN([1,2,3,4,5,6,7], 2, 1, 1, [], X).

%crearCartaN2
%DOM: list X int X int X int X int X list X variable
%REC: list
%Recursión: De cola
%%Resumen: Hecho que crea una "cartaN^2" de un CardsSet.
%Caso Base
crearCartaN2(Elementos, I, J, N, N, Carta, R):-
    Num is (N+2+N*(N-1)+(((I-1)*(N-1)+J-1) mod N)), 
    nth1(Num, Elementos, X),
    append(Carta, [X], CartaN),
    R = CartaN, !.
%Caso Vacío
crearCartaN2(Elementos, I, J, K, N, [], R):-
    I1 is (I + 1), 
    nth1(I1, Elementos, Carta),
    crearCartaN2(Elementos, I, J, K, N, [Carta], R),!.
%Caso Recursivo
crearCartaN2(Elementos, I, J, K, N, Carta, R):- 
	Num is (N+2+N*(K-1)+(((I-1)*(K-1)+J-1) mod N)), 
    nth1(Num, Elementos, X),
    append(Carta, [X], CartaN),
    K1 is (K+1),
    crearCartaN2(Elementos, I, J, K1, N, CartaN, R).
%Ejemplo de uso: 
%crearCartaN2([1,2,3,4,5,6,7,8,9,10,11], 1, 1, 1, 3, [], R).

%cicloCrearCartasN2
%DOM: list X int X int X int X int X list X variable
%REC: list
%Recursivo: De Cola
%Resumen: Ciclo que crea todas las "cartasN^2" de un cardsSet
%Caso Base (I = J = N)
cicloCrearCartasN2(Elementos, N, N, K, N, CardsSet, R):-
    crearCartaN2(Elementos, N, N, K, N, [], X),
    append(CardsSet, [X], Y),
    R = Y.
%Caso J = N
cicloCrearCartasN2(Elementos, I, N, K, N, CardsSet, R):-
    crearCartaN2(Elementos, I, N, K, N, [], X),
    append(CardsSet, [X], Y),
    (I1 is (I+1)),
    cicloCrearCartasN2(Elementos, I1, 1, K, N, Y, R).
%Caso Recursivo
cicloCrearCartasN2(Elementos, I, J, K, N, CardsSet, R):-
    crearCartaN2(Elementos, I, J, K, N, [], X),
    append(CardsSet, [X], Y),
    J1 is (J+1),
    cicloCrearCartasN2(Elementos, I, J1, K, N, Y, R), !.
%Ejemplo de uso: 
%cicloCrearCartasN2([1,2,3,4,5,6,7,8,9,10,11,12,13], 1, 1, 1, 3, [], R).

%cortarLista
%DOM: list X int X list X Variable
%REC: list
%Recursión: De cola
%Resumen: Corta una lista hasta "C"
cortarLista(_, 0, ListaNueva, ListaNueva):- !.
cortarLista([CAR|CDR], C, ListaNueva, R):-
    append(ListaNueva, [CAR], Lista),
    C1 is (C-1),
    cortarLista(CDR, C1, Lista, R).
%Ejemplo de uso: cortarLista([1,2,3,4,5], 3, [], X).

%cardsSet
%Caso maxC = -1
cardsSet(Elementos, NumE, -1, rndFn(), CardsSet, CS):-
	calcular_orden(NumE, O),
    MaxC is ((O*O)+O+1),
    cardsSet(Elementos, NumE, MaxC, rndFn(), CardsSet, CS), !.
%Caso Vacío []: crea un cardsSet full
cardsSet(Elementos, NumE, MaxC, rndFn(), [] ,CS):-
    calcular_orden(NumE, Orden),
    crearCarta1(Elementos, NumE, [], X),
    cicloCrearCartasN(Elementos, Orden, 1, 1, [], CN),
    append([X], CN, Y),
    cicloCrearCartasN2(Elementos, 1, 1, 1, Orden, [], CN2),
    append(Y, CN2, Z),
    cardsSet(Elementos, NumE, MaxC, rndFn(), Z ,CS),!.
%Caso Base: Corta el cardsSet full hasta maxC 
cardsSet(_, _, MaxC, rndFn(), CardsSet, CS):-
    cortarLista(CardsSet, MaxC, [], CardsSetCortado),
    CS = CardsSetCortado.
%Ejemplos de uso: 
%cardsSet([1,2,3,4,5,6,7], 3, 2, rndFn(), [], CS).
%cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13], 4, 2, rndFn(), [], CS).
%cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13], 4, -1, rndFn(), [], CS).

% Ejemplo ciclo iterativo
iterativo(N,N,X,X):-!.
iterativo(I,N,X,Resultado):-
	X2 is (X+5),
    I1 is (I+1),
    iterativo(I1,N,X2,Resultado).
