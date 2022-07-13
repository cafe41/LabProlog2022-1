% PARADIGMAS DE LA PROGRAMACIÓN
% SECCIÓN DEL CURSO: 0-B-2
% PROFESOR DE TEORÍA: VICTOR FLORES
% PROFESOR DE LABORATORIO: VICTOR FLORES
%
% AUTOR
% NOMBRE: Gustavo Andrés Vergara Parra
% RUT: 19.810.849-9
% CARRERA: Ingenieria en Ejecucion en Informatica
% VERSIÓN SWI-PROLOG: 8.4.2

%HECHOS

%REGLAS

%PREDICADOS

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
%Resumen: Predicado que calcula el Orden de un cardsSet
calcular_orden(NumE, Orden):-
    Orden is NumE-1.
%Ejemplo de uso: calcular_orden(4, Orden),

%crearCarta1
%Dominio: list X int X list X variable
%Recorrido: list
%Recursión: de Cola
%Predicado que crea la primera carta de un CardsSet.
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
%Predicado que crea una "cartaN" de un CardsSet.
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
%%Resumen: Predicado que crea una "cartaN^2" de un CardsSet.
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
%Caso Recursivo
cortarLista([CAR|CDR], C, ListaNueva, R):-
    append(ListaNueva, [CAR], Lista),
    C1 is (C-1),
    cortarLista(CDR, C1, Lista, R).
%Caso Base
cortarLista(_, 0, ListaNueva, ListaNueva):- !.
%Ejemplo de uso: cortarLista([1,2,3,4,5], 3, [], X).

%cardsSet
%DOM: Elements (list) X numE(int) X maxC(int) X seed (int) X CS (CardSet)
%REC: list (con listas)
%Recursión: De Cola
%Resumen: Genera un conjunto de cartas a partir de una lista
%de elementos, un "NumE", un "MaxC" y un "Seed"
%Caso maxC = -1
cardsSet(Elementos, NumE, -1, rndFn(), _, CS):-
	calcular_orden(NumE, O),
    MaxC is ((O*O)+O+1),
    cardsSet(Elementos, NumE, MaxC, rndFn(), _, CS), !.
%Caso Vacío []: crea un cardsSet full
cardsSet(Elementos, NumE, MaxC, rndFn(), _, CS):-
    calcular_orden(NumE, Orden),
    crearCarta1(Elementos, NumE, [], X),
    cicloCrearCartasN(Elementos, Orden, 1, 1, [], CN),
    append([X], CN, Y),
    cicloCrearCartasN2(Elementos, 1, 1, 1, Orden, [], CN2),
    append(Y, CN2, Z),
    cortarLista(Z, MaxC, [], CardsSetCortado),
    CS = CardsSetCortado, !.
    
%Ejemplos de uso: 
%cardsSet([1,2,3,4,5,6,7], 3, 2, rndFn(), 1234, CS).
%cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13], 4, 2, rndFn(), 4321, CS).
%cardsSet([1,2,3,4,5,6,7,8,9,10,11,12,13], 4, -1, rndFn(), 92175, CS).

%contains
%DOM: list X element
%REC: boolean
%Recursión: de cola
%Resumen: Revisa si una lista contiene un elemento ( =:= )
%Caso Recursivo
contains([CAR|CDR], Elemento):-
	CAR =:= Elemento, !;
    contains(CDR, Elemento).
%Caso Base
contains([], _):-
    false, !.
%Ejemplo de uso: contains([1,2,3,4], 4).

%elementosRepetidos
%DOM: list X Variable
%REC: boolean
%Recursión: De cola
%Resumen: Predicado que busca elementos repetidos dentro de una lista,
%Si existe un elemento repetido retornará "true", sino "false"
elementosRepetidos([CAR|CDR], ListaElementos):-
    %Agregamos los elementos a una lista, si esta no contiene
    contains(ListaElementos, CAR), !;
    append(ListaElementos, [CAR], ListaElementosNueva),
    elementosRepetidos(CDR, ListaElementosNueva).
%Caso Base
elementosRepetidos([], _):- false, !.
%Ejemplo de uso: 
%elementosRepetidos([1,2,3,4,5,6], []). %True
%elementosRepetidos([1,2,3,5,5,6], []). %False

%elementoEnComun
%DOM: list X list X int X Variable
%REC: int
%Recursión: De cola
%Resumen: Predicado que retorna la cantidad de elementos en común entre
%         dos listas.
%Caso Base
elementosEnComun(_, [], Comun, Elementos):-
    Comun = Elementos, !.
%Caso Recursivo
elementosEnComun(Lista, [CAR|CDR], Comun, Elementos):-
    %Si la lista contiene el elemento de la otra, le agrega 1
    contains(Lista, CAR), Comun1 is (Comun+1),
    elementosEnComun(Lista, CDR, Comun1, Elementos), !;
    %Sino, simplemente continúa
    elementosEnComun(Lista, CDR, Comun, Elementos), !.
%Ejemplo de uso: elementosEnComun([1,2,3,4,5], [1,3,6,7,8], 0,  Elementos).

%cardsSetIsDobble
%DOM: cardsSet
%REC: boolean
%Recursión: De cola.
%Resumen: Predicado que permite verificar si el conjunto de cartas en el 
%TDA corresponden a un conjunto válido. Esto es, que en cada tarjeta se 
%listan n elementos (figuras, números, letras, etc.) diferentes. Además, 
%para cualquier par de cartas, existe un y sólo un elemento en común. Si 
%se cumplen estascondiciones el predicado retorna true, de lo contrario 
%false.
%Caso Recursivo
cardsSetIsDobble([Carta1|[Carta2|Mazo]]):- 
    %Si no hay elementos repetidos,
    not(elementosRepetidos(Carta1, [])),
    %Y si la  cantidad de elementos en comun <= 1,
    elementosEnComun(Carta1, Carta2, 0,  Elementos),
    Elementos =:= 1, %Si solo tienen 1 elemento en común
    %Se hace un llamado recursivo con las siguientes dos cartas
  	cardsSetIsDobble([Carta2|Mazo]), !.
%Caso Ultima carta
cardsSetIsDobble([Carta1|[]]):- 
    %Solo revisará si NO hay elementos repetidos y se dará el resultado.
    not(elementosRepetidos(Carta1, [])).
%Ejemplos de uso:
%cardsSetIsDobble([[1,2,3],[1,4,5],[1,6,7],[2,4,6],[2,5,7]]). %Caso true
%cardsSetIsDobble([[1,2,3], [1,4,5], [2,4,6]]). %Caso True
%cardsSetIsDobble([[1,2,3], [2,2,3], [3,4,5]]). %Caso False

%cardsSetNthCard
%DOM: cardsSet X Entero X card
%REC: card
%Recursión: De Cola 
%Resumen: Busca la carta "n" dentro del cardsSet, contando de 0 a n.
%Caso Base
cardsSetNthCard([CAR|_], 0, Card):-
    CAR = Card, !.
%Caso Recursivo
cardsSetNthCard([_|CDR], N, Card):-
    Nmenos1 is (N-1),
    cardsSetNthCard(CDR,Nmenos1,Card).
%Ejemplos de uso:
%cardsSetNthCard([[1,2,3],[1,4,5],[1,6,7],[2,4,6],[2,5,7]], 3, Card).
%cardsSetNthCard([[1,2,3],[1,4,5],[1,6,7],[2,4,6],[2,5,7]], 0, Card).
%cardsSetNthCard([[1,2,3],[1,4,5]], 1, Card).

%cardsSetFindTotalCards
%DOM: card X Int
%REC: Int
%Recursión: De cola
%Resumen: A partir de una carta de muestra, determina la cantidad total de 
%cartas que se deben producir para construir un conjunto válido.
cardsSetFindTotalCards(Card, Int):-
    length(Card, N),
    calcular_orden(N, N1),
    N2 is (N1*N1),
    Int is (N2 + N).
%El total de cartas es n^2 + n + 1.
%Ejemplos de uso: 
%cardsSetFindTotalCards([1,2,3],Int).
%cardsSetFindTotalCards([1,4,5], Int).
%cardsSetFindTotalCards([1,2,3,4,5,6,7], Int).

%obtenerListaElementos
%Caso Recursivo
obtenerListaElementos([[CAR|CDRCAR]|CDR], ListaElementos, Lista):-
    %Si la lista no contiene al elemento, entonces le hace append
    not(contains(ListaElementos, CAR)), 
    append(ListaElementos, [CAR], ListaNueva),
    obtenerListaElementos([CDRCAR|CDR], ListaNueva, Lista),!;
    %Sino, ignora el elemento
    obtenerListaElementos([CDRCAR|CDR], ListaElementos, Lista).
%Caso Carta Vacía
obtenerListaElementos([[]|CDR], ListaElementos, Lista):-
    obtenerListaElementos(CDR, ListaElementos, Lista),!.
%Caso CardsSet Vacío (Caso Base)
obtenerListaElementos([], Lista, Lista).
%Ejemplo de uso: obtenerListaElementos([[1,2,3],[1,5,6]],[],ListaElementos).

%cardsSetMissingCards
%DOM: cardsSet X cardsSet
%REC: cardsSet
%Recursión: 
%Resumen: A partir de un conjunto de cartas retorna el conjunto de cartas 
%que hacen falta para que el set sea válido.
cardsSetMissingCards([CAR|CDR], CardsSetSalida):-
    %Si es un cardsSet Válido:
    cardsSetIsDobble([CAR|CDR]),
    %Obtenemos una lista con todos los elementos del CardsSet
    obtenerListaElementos([CAR|CDR], [], ListaE),
    %Si el largo de la lista de elementos es mayor o igual a la cantidad
    %total de cartas para un CardsSet ideal, entonces creamos un cardsSet.
    length(ListaE, Largo),  cardsSetFindTotalCards(CAR, TotalCards),
    Largo >= TotalCards, length(CAR, NumE),
    cardsSet(ListaE, NumE, -1, rndFn(), 0, CardsSetSalida).
    %Retorna false si la lista de elementos no es suficiente.
%Ejemplos de uso:
%cardsSetMissingCards([[1,2,3],[1,4,5],[1,6,7]], CardsSetIdeal).
%cardsSetMissingCards([[1,4,5],[2,4,6],[3,4,7]], CardsSetIdeal).
%cardsSetMissingCards([[1,2,3,4],[1,5,6,7],[1,8,9,10],[1,11,12,13]], CardsSetIdeal).

%cardsSetToString
%DOM: cardsSet X string
%REC: String
%Recursión: No.
%Resumen: convierte un conjunto de cartas a una representación basada en 
%strings que posteriormente pueda visualizarse a través del predicado write.
cardsSetToString(CardsSet, CardsSetString):-
    term_string(CardsSet, CardsSetString).
%Ejemplos de uso:
% cardsSetToString([[1,2,3],[1,4,5],[1,5,6]], CardSetString).
% cardsSetToString([[1,2,3],[1,5,6]], CardSetString).
% cardsSetToString([[1,2,3],[1,4,5]], CardSetString), write(CardSetString).

%dobbleGame
%DOM: numPlayers(int) X cardsSet X mode(string) X seed(int) X game(TDA Game)
%REC: game
%Recursión: No
%Resumen: Constructor del TDA Game.
dobbleGame(NumPlayers, CardsSet, Mode, _, Game):-
    append([NumPlayers],[CardsSet],GameNC),
    append(GameNC,[Mode],GameNCM),
    append(GameNCM, [[]], Game).
%Ejemplos de uso:
%dobbleGame(3,[[1,2,3,4],[1,5,6,7],[1,8,9,10],[1,11,12,13]],"StackMode",0,TDAGame).
%dobbleGame(3,[[1,2,3],[1,4,5],[1,5,6]],"CPUMode",1920,Game).
%dobbleGame(7,[[1,2,3],[1,4,5]],"ModoPersonalizado",1234,Game).

%Getters, Dominio: Game X Variable
getNumPlayers([CAR|_], NumPlayers):- NumPlayers is CAR.
getCardsSet([_,CardsSet,_,_], CardsSet).
getModo([_,_,Modo,_],Modo).
getPlayers([_,_,_,Players], Players).
%Ejemplo de uso:
%dobbleGame(7,[[1,2,3],[1,4,5]],"ModoPersonalizado",1234,Game), getNumPlayers(Game, NumPlayers), getCardsSet(Game, CardsSet), getModo(Game, ModoDeJuego), getPlayers(Game, Jugadores).

%Setters, 
%Dominio: Game X Int X Variable
setNumPlayers([_|CDR], NumP, GameOut):- append([NumP],CDR,GameOut).
%Dominio: Game X CardsSet X Variable
setCardsSet(GameIn, CardsSet, GameOut):-
    getNumPlayers(GameIn, NumP),
	getModo(GameIn, Modo),
	getPlayers(GameIn, Jugadores),
    append([NumP,CardsSet,Modo,Jugadores],[],GameOut).
%Dominio: Game X Modo(String) X Variable
setModo(GameIn, Modo, GameOut):-
    getNumPlayers(GameIn, NumP),
	getCardsSet(GameIn, CardsSet),
	getPlayers(GameIn, Jugadores),
    append([NumP,CardsSet,Modo,Jugadores],[],GameOut).
%Dominio: Game X Players(list) X Variable
setPlayers(GameIn, Jugadores, GameOut):-
    getNumPlayers(GameIn, NumP),
	getCardsSet(GameIn, CardsSet),
    getModo(GameIn, Modo),
    append([NumP,CardsSet,Modo,Jugadores],[],GameOut).
%Ejemplo de usos: dobbleGame(7,[[1,2,3],[1,4,5]],"ModoPersonalizado",1234,Game), setNumPlayers(Game, 1, GameOut1),setCardsSet(GameOut1, [[1,2,3,4],[1,5,6,7]], GameOut2),setModo(GameOut2, "StackMode", GameOut3),setPlayers(GameOut3, [["Pedro", 0]], GameOut4).

%revisarJugadores
%DOM: Game X Usuario
%REC: boolean
%Recursión: De cola
%Resumen: Revisa que el jugador no esté en la lista, si es así, retorna true.
revisarJugadores(Game, User):-
    getPlayers(Game, [[CARCAR|_]|CDR]),
    (not(CARCAR == User)),
    setPlayers(Game, CDR, Game2),
    revisarJugadores(Game2, User), !.
revisarJugadores([_,_,_,[]], _):- true, !.
%Ejemplo de uso: revisarJugadores([7, [[1, 2, 3]], "Modo", [["cafe41", 0]]], "cafe41").

%dobbleGameRegister
%DOM: user(String) X gameIn (Game) X gameOut (Game)
%REC: GameOut
%Recursión: 
%Resumen: Predicado que registra un jugador en el TDAGame, los jugadores 
%tienen un nombre único y no puede exceder la cantidad de jugadores 
%registrados.
dobbleGameRegister(Usuario, GameIn, GameOut):-
    %Si la lista de jugadores NO contiene al jugador ya,
    getPlayers(GameIn, Players),
    revisarJugadores(GameIn, Usuario),
    %Segunda parte: revisamos que no está "llena"
    length(Players, LargoP),
    getNumPlayers(GameIn, NumP),
    LargoP < NumP,
    append(Players, [[Usuario, 0]], PlayersActual),
    setPlayers(GameIn, PlayersActual, GameOut).
%Ejemplos de uso: 
%dobbleGameRegister("cafe41", [2, [[1, 2, 3], [1, 4, 5]], "StackMode", []], GameOut).
%dobbleGameRegister("cafe41", [2, [[1, 2, 3], [1, 4, 5]], "StackMode", [["cafe41",0]]], GameOut).
%dobbleGameRegister("perro", [2, [[1, 2, 3], [1, 4, 5]], "StackMode", [["cafe41",0]]], GameOut).

%dobbleGameWhoseTurnIsIt
%DOM: game (Game) X username (string)
%REC: string
%Recursión: No
%Resumen: Predicado que permite obtener el usuario a quién le corresponde 
%jugar en el turno.
dobbleGameWhoseTurnIsIt(Game, UsuarioActual):-
    getPlayers(Game, [[UsuarioActual|_]|_]).    
%Ejemplos de uso:
% dobbleGameWhoseTurnIsIt([2, [[1, 2, 3], [1, 4, 5]], "StackMode", [["cafe41", 0]]], User).
% dobbleGameWhoseTurnIsIt([2, [[1, 2, 3], [1, 4, 5]], "StackMode", [["cafe41", 0], ["perro", 0]]], Usuario)
% dobbleGameWhoseTurnIsIt([3, [], "StackMode", [["Galleta",0],["cafe41",0]]], User) 

%dobbleGamePlay

%dobbleGameStatus

%dobbleGameScore

%dobbleGameToString
%DOM: game (Game) X str (string)
%REC: str
%Recursión: 
%Resumen: Predicado que permite relacionar un TDA de juego con su 
%representación como string de este, esto para posteriormente pueda 
%visualizarse a través de la función display.
dobbleGameToString(Game, GameToString):-
    getNumPlayers(Game, NP), term_string(NP, NPString),
    getCardsSet(Game,CS), term_string(CS, CSString),
    getModo(Game,M), term_string(M, MString),
    getPlayers(Game, P), term_string(P, PString),
    string_concat(NPString, '\n', NPSn), 
    string_concat(CSString, '\n', CSSn),
    string_concat(MString, '\n', MSn), 
    string_concat(NPSn, CSSn, NPSCSS),
    string_concat(MSn, PString, MPS),
    string_concat(NPSCSS, MPS, GameToString).
%Ejemplos de uso:
% dobbleGameToString([2, [[1, 2, 3], [1, 4, 5]], "StackMode", [["cafe41", 0], ["Galleta", 0]]], GTS).
% dobbleGameToString([2, [[1, 2, 3], [1, 4, 5]], "StackMode", [["cafe41", 0], ["uwu", 0]]], GTS), write(GTS).
% dobbleGameToString([7, [[1, 2, 3], [1, 4, 5]], "ModoPersonalizado", []], String), write(String).