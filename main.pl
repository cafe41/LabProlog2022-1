% PARADIGMAS DE LA PROGRAMACIÓN
% SECCIÓN DEL CURSO: 0-B-2
% PROFESOR DE TEORÍA: VICTOR FLORES
% PROFESOR DE LABORATORIO: VICTOR FLORES
%
% AUTOR
% NOMBRE: Gustavo Andrés Vergara Parra
% RUT: 19.810.849-9
% CARRERA: Ingeniería en Ejecución en Informática

% Copypasteamos para ver la lista completa:
% set_prolog_flag(answer_write_options,[max_depth(0)]). true.

% Importamos los otros archivos, creo.
% ["baseDeDatos.pl"].
% consult(tda-cardset.pl).
% consult(tda-game.pl).

%

%Base de datos


%Reglas
% cards():-

%EJEMPLO DE USO:  addElemento1(3, [1, 2, 4], X).
addElemento1(Elemento, [], [Elemento]):- !.
addElemento1(Elemento, [C|R], [C|L]):-
    A is Elemento,
    addElemento1( A, R, L). % Donde R es el resto y L es la lista.

crearCarta1([], 0).
crearCarta1([CAB|COL], [PrimE|UltE], NumE):-
    addElemento1(A,[CAB|COL], [H|T]),
    Emenos1 is NumE -1,
    crearCarta1([H|T], UltE, Emenos1),
    PrimE = H.

% crearCartaN("cartaN"):-

%No sé usar esto, ayuda