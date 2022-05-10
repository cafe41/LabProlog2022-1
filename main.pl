% PARADIGMAS DE LA PROGRAMACIÓN
% SECCIÓN DEL CURSO: 0-B-2
% PROFESOR DE TEORÍA: VICTOR FLORES
% PROFESOR DE LABORATORIO: VICTOR FLORES
%
% AUTOR
% NOMBRE: Gustavo Andrés Vergara Parra
% RUT: 19.810.849-9
% CARRERA: Ingeniería en Ejecución en Informática
% VERSIÓN PROLOG: 8.4.2

% Copypasteamos para ver la lista completa:
% set_prolog_flag(answer_write_options,[max_depth(0)]), true.

% Importamos los otros archivos, creo.
% ["baseDeDatos.pl"].
% consult(tda-cardset.pl).
% consult(tda-game.pl).

%

%Base de datos


%REGLAS

%Dominio: int X list X variable
%Ejemplo de uso:  addElemento1(3, [1, 2, 4], X).
%Recorrido: list
%Recursión: Natural
addElemento1(Elemento, [], [Elemento]):- !.
addElemento1(Elemento, [C|R], [C|L]):-
    A is Elemento,
    addElemento1( A, R, L). % Donde R es el resto y L es la lista.

%calcularOrden(MaxC, Orden).
%Dominio: int X variable
%Ejemplo de uso: calcularOrden(7, O).
%Recorrido: int
%Recursión: No hay
calcularOrden(MaxC, Orden):-
    R is sqrt(1 - 4*(-1 * MaxC)),
    R is R - 1,
    R is R/2.

%Dominio: list X int X variable
%Ejemplo de uso: crearCarta1(["a","b","c"], 3, X)
%Recorrido: list
%Recursión: Natural
crearCarta1([], 0, _):- !.
crearCarta1([CAB|COL], Orden, [PrimE|UltE]):-
    addElemento1(CAB, [PrimE|UltE], X),
    Omenos1 is Orden -1,
    crearCarta1([COL], Omenos1, X).

% crearCartaN("cartaN"):-

% crearCartaN2("cartaN2"):-

cardsSet(Elementos, -1, MaxC, rndFn(), CS):- 
    NumE is calcularOrden(MaxC, O),
    cardsSet(Elementos, NumE, MaxC, rndFn(), CS).
cardsSet(Elementos, NumE, MaxC, rndFn(), CS):-
    orden is NumE-1,
    crearCarta1(Elementos, orden, X).
%  crearCartaN().
%  crearCartaN2(). 