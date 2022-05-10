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


%Reglas
/* 
cardsSet():- cardsSet()
cardsSet():-
    orden is numE-1
    
*/

%EJEMPLO DE USO:  addElemento1(3, [1, 2, 4], X).
addElemento1(Elemento, [], [Elemento]):- !.
addElemento1(Elemento, [C|R], [C|L]):-
    A is Elemento,
    addElemento1( A, R, L). % Donde R es el resto y L es la lista.

%Dominio: list X int X variable ("elementos", numE, X) 
%Ejemplo de uso: crearCarta1(["a","b","c"], 3, X)
%Recorrido: list ([PrimE|UltE])
%Recursión: Natural
crearCarta1([], 0 ,X).
crearCarta1([CAB|COL], Orden, [PrimE|UltE]):-
    addElemento1(CAB, [PrimE|UltE], X),
    Omenos1 is Orden -1,
    crearCarta1([H|T], Emenos1, X).

% crearCartaN("cartaN"):-
