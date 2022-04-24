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
consult(baseDeDatos.pl).
consult(tda-cardset.pl).
consult(tda-game.pl).

%

%Base de datos


%Reglas
% cards():-

addElemento1(A, B, [_|_]):-
    A is elemento(_).

crearCarta1( [], 0, _).
crearCarta1( [PrimE|UltE], numE, cantCartas):- 
    addElemento1(A,B),
    Emenos1 is numE -1,
    PrimE is B.



% crearCartaN("cartaN"):-

%No sé usar esto, ayuda