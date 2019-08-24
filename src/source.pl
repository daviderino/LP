% David Baptista 92446

:- consult(codigo_comum).
:- consult('testes_publicos\\puzzles_publicos.pl').

/*----------------------------------------------------------------
cont_vars(L, Cont): Cont e o numero resultante de contar as
variaveis presentes na lista L
----------------------------------------------------------------*/
cont_vars(L, Cont) :-
    include(var, L, L_vars),
    length(L_vars, Cont).

/*----------------------------------------------------------------
fill_one(Lista, Res): Res e o resultado de preencher Lista 
com o valor correto (1 caso haja dois 0's, 0 caso haja dois 1's)
----------------------------------------------------------------*/
fill_one([A, B, C], Res):-
    (var(C), A == 1, B == 1, NC is 0, Res = [A, B, NC], !
    ;
    var(C), A == 0, B == 0, NC is 1, Res = [A, B, NC], !
    ;
    var(B), A == 1, C == 1, NB is 0, Res = [A, NB, C], !
    ;
    var(B), A == 0, C == 0, NB is 1, Res = [A, NB, C], !
    ;
    var(A), B == 1, C == 1, NA is 0, Res = [NA, B, C], !
    ;
    var(A), B == 0, C == 0, NA is 1, Res = [NA, B, C], !)
    ;
    Res = [A, B, C], !.

/*----------------------------------------------------------------
head_tail(Lista, X, Y): X e a cabeca de Lista (primeiro elemento)
e Y e a cauda da lista (restantes elementos)
----------------------------------------------------------------*/
head_tail([H|T], X, Y) :-
    X = H,
    Y = T.

/*----------------------------------------------------------------
valores_iguais(X1, X2): e true caso X1 e X2 sejam ambos variaveis
ou tenham ambos o mesmo valor.
----------------------------------------------------------------*/
valores_iguais(X1, X2) :-
    (var(X1), var(X2), true, !
    ;
    X1 == X2, !).

/*----------------------------------------------------------------
listas_iguais(L1, L2): compara L1 e L2 e e true caso sejam listas
iguais
----------------------------------------------------------------*/
listas_iguais([], []).

listas_iguais([H1|T1], [H2|T2]) :-
    valores_iguais(H1, H2),
    listas_iguais(T1, T2).

/*----------------------------------------------------------------
head_tail(P1, P2): Compara P1 e P2 e e true caso sejam puzzles 
iguais
----------------------------------------------------------------*/
puzzles_iguais([H1|T1], [H2|T2]) :-
    (listas_iguais(H1, H2), true, ! ; false, !),
    (length(T1, 0), true, ! ; puzzles_iguais(T1, T2), !).

/*----------------------------------------------------------------
count(L, Valor, Acum): Acum e o numero de vezes que Valor se repete
na lista L
----------------------------------------------------------------*/
count([], _, 0).

count([H|T], Valor, Acum) :-
    count(T, Valor, N),
    (number(H), H == Valor, Acum is N + 1, !
    ;
    Acum = N, !).

/*----------------------------------------------------------------
fill_with(L, Val, Ac, Res): Res e o resultado de preencher
as posicoes em branco da Lista L com o valor Val
----------------------------------------------------------------*/
fill_with([H|T], Val, Ac, Res) :-
    (var(H), A is Val, !
    ; 
    A is H, !),
    flatten([Ac, [A]], N_Ac),
    (length(T, 0), Res = N_Ac, ! 
    ; 
    fill_with(T, Val, N_Ac, Res), !).

/*----------------------------------------------------------------
valores_R3_iguais(X1, X2): e true caso X1 e X2 sejam iguais segundo
as regras usadas no aplica_R3
----------------------------------------------------------------*/
valores_R3_iguais(X1, X2) :-
    (nonvar(X1), nonvar(X2), X1 == X2, !
    ;
    nonvar(X2), nonvar(X1), X1 == X2, !
    ; 
    !, false).

/*----------------------------------------------------------------
get_coors(Coors, X, Y): dado um tuplo de coordenadas
Coors, X e Y sao os respetivos valores da coordenada
----------------------------------------------------------------*/
get_coords((A,B), X, Y) :-
    X = A,
    Y = B.

/*----------------------------------------------------------------
get_linha(Puz, Index, Linha): Linha e a respetiva linha numero
Index do Puzzle Puz
----------------------------------------------------------------*/
get_linha(Puz, Index, Linha) :-
    nth1(Index, Puz, Linha).

/*----------------------------------------------------------------
listas_R3_iguais(L1, L2): e true caso L1 e L2 sejam listas iguais
segundo os standards do R3
----------------------------------------------------------------*/
listas_R3_iguais([], []).

listas_R3_iguais([H1|T1], [H2|T2]) :-
    valores_R3_iguais(H1, H2),
    listas_R3_iguais(T1, T2).

/*----------------------------------------------------------------
aux_get_alteracoes(P1, P2, X, Y, line, Ac, Res): Res e a lista 
resultante de verificar quais as alteracoes feitas entre o puzzle
P1 e P2 na linha X ou na coluna Y consoante o valor de Line
----------------------------------------------------------------*/
aux_get_alteracoes(P1, P2, X, Y, Line, Ac, Res) :-
    (Line == 1, mat_dimensoes(P1, _, Max), ! 
    ; 
    mat_dimensoes(P1, Max, _), !),

    M is Max+1,
    (M > Y,
        (
            (Line == 1, mat_ref(P1, (X, Y), Val1), mat_ref(P2, (X, Y), Val2), ! 
            ; 
            mat_ref(P1, (Y, X), Val1), mat_ref(P2, (Y, X), Val2), !),

            (var(Val1),
                (var(Val2), 
                    Temp = Ac, !
                    ; 
                    (Val1 == Val2, 
                        Temp = Ac, !
                        ;    
                        (Line == 1, 
                            flatten([Ac, [(X,Y)]], Temp), ! 
                            ; 
                            flatten([Ac, [(Y,X)]], Temp), !
                        )
                    )
                )
                ;
                (Val1 == Val2, 
                    Temp = Ac, !
                    ; 
                    (Line == 1, 
                        flatten([Ac, [(X,Y)]], Temp), ! 
                        ; 
                        flatten([Ac, [(Y,X)]], Temp), !
                    )
                )
            ),

            Z is Y + 1,
            aux_get_alteracoes(P1, P2, X, Z, Line, Temp, Res)
        ), !
        ;
        Res = Ac, !
    ).

get_alteracoes(P1, P2, X, Line, Coors) :-
    aux_get_alteracoes(P1, P2, X, 1, Line, [], Coors).

/*----------------------------------------------------------------
get_empty_linha(Puz, Iterator, Last_Col, X, Ac, Res): Res e o 
resultado de encontrar as posicoes vazias do puzzle Puz
----------------------------------------------------------------*/
get_empty_linha(Puz, Iterator, Last_Col, X, Ac, Res) :- 
    mat_ref(Puz, (X, Iterator), Val),
    (Iterator == Last_Col,
        (
            var(Val), flatten([Ac, [(X, Iterator)]], Res), ! 
            ; 
            Res = Ac, !
        ), !
        ; 
        (
            (var(Val), flatten([Ac, [(X, Iterator)]], N_Ac), ! 
            ; 
            N_Ac = Ac, !),
            I is Iterator+1, 
            get_empty_linha(Puz, I, Last_Col, X, N_Ac, Res)
        ), !
    ).

get_empty(Puz, Current_Row, N_Rows, N_Cols, Ac, Res) :-
    get_empty_linha(Puz, 1, N_Cols, Current_Row, [], N_Ac),

    (Current_Row == N_Rows, flatten([Ac, N_Ac], R_Ac), Res = R_Ac, !
        ; 
        N_C is Current_Row+1,
        flatten([Ac, N_Ac], R_Ac),
        get_empty(Puz, N_C, N_Rows, N_Cols, R_Ac, Res), !).

/*----------------------------------------------------------------
aplica_R1_triplo(Triplo, N_Triplo): N_Triplo e a lista resultante 
de aplicar a regra 1 ao triplo Triplo. Se o Triplo tiver tres
zeros (uns), o predicado devolve false.
----------------------------------------------------------------*/
aplica_R1_triplo([A, B, C], N_Triplo) :-
    cont_vars([A, B, C], Num_Vars),
    (Num_Vars == 1, fill_one([A, B, C], N_Triplo), !
    ;
    Num_Vars == 0, A == B, B == C, !, fail
    ;
    N_Triplo = [A, B, C]).

/*----------------------------------------------------------------
aplica_R1_fila_aux(Fila, N_Fila), Fila e uma fila (linha ou coluna)
de um puzzle, N_Fila e a fila resultante de aplicar a regra 1 
a fila Fila, uma so vez. Se a Fila tiver tres zeros (uns) seguidos
, o predicado devolve false.
----------------------------------------------------------------*/
aplica_R1_fila_aux([A, B, C], N_Fila) :-
    aplica_R1_triplo([A, B, C], N_Fila), !.

aplica_R1_fila_aux([A, B, C | R], N_Fila) :-
    aplica_R1_triplo([A, B, C], [NA, NB, NC]),
    aplica_R1_fila_aux([NB, NC |R], N_Temp),
    N_Fila = [NA | N_Temp], !.

/*----------------------------------------------------------------
aplica_R1_fila(Fila, N_Fila), Fila e uma fila (linha ou coluna) de
um puzzle, N_Fila e a fila resultante de aplicar a regra 1 a fila Fila.
Se a Fila tiver tres zeros (uns) seguidos, o predicado devolve false.
----------------------------------------------------------------*/
aplica_R1_fila(Fila, N_Fila) :-
    aplica_R1_fila_aux(Fila, T_Fila),
    (listas_iguais(Fila, T_Fila), N_Fila = T_Fila, !
    ;
    aplica_R1_fila_aux(T_Fila, N_Fila), !).
x
/*----------------------------------------------------------------
aplica_R2_fila(Fila, N_Fila), em que Fila e uma fila de um puzzle, 
significa que N_Fila e a fila resultante de aplicar a regra 2 a fila Fila.
Se o numero de zeros ou uns ultrapassar N , o predicado devolve false.
----------------------------------------------------------------*/
aplica_R2_fila_aux(Fila, N_Fila) :-
    length(Fila, L),
    N is L/2,
    count(Fila, 0, N0),
    count(Fila, 1, N1),
    (N < N0, !, false ; true, !),
    (N < N1, !, false ; true, !),
    (N == N0, fill_with(Fila, 1, [], N_Fila), ! ; N == N1, fill_with(Fila, 0, [], N_Fila), ! ; N_Fila = Fila, !).

aplica_R2_fila(Fila, N_Fila) :-
    aplica_R2_fila_aux(Fila, T1),
    aplica_R2_fila_aux(T1, T2),
    (listas_iguais(T1, T2), aplica_R2_fila_aux(Fila, N_Fila), ! ; aplica_R2_fila_aux(Fila, Temp), aplica_R2_fila(Temp, N_Fila)).

/*----------------------------------------------------------------
aplica_R1_R2_fila(Fila, N_Fila),Fila e uma fila de um puzzle,
N_Fila e a fila resultante de aplicar as regras 1 e 2 a fila Fila,
por esta ordem.
----------------------------------------------------------------*/
aplica_R1_R2_fila(Fila, N_Fila) :-
    aplica_R1_fila(Fila, Temp),
    aplica_R2_fila(Temp, N_Fila).

/*----------------------------------------------------------------
aplica_R1_R2_puzzle(Puz, N_Puz), em que Puz e um puzzle, N_Puz
e o puzzle resultante de aplicar o predicado aplica_R1_R2_fila, 
as linhas e as colunas de Puz.
----------------------------------------------------------------*/
aplica_R1_R2_puzzle_aux([], Ac, Ac).

aplica_R1_R2_puzzle_aux([H|T], Ac, N_Puz) :-
    aplica_R1_R2_fila(H, Temp),
    append(Ac, [Temp], Res),
    aplica_R1_R2_puzzle_aux(T, Res, N_Puz).

aplica_R1_R2_puzzle(Puz, N_Puz) :-
    aplica_R1_R2_puzzle_aux(Puz, [], T1),
    mat_transposta(T1, T2),
    aplica_R1_R2_puzzle_aux(T2, [], T3),
    mat_transposta(T3, N_Puz).

/*----------------------------------------------------------------
inicializa(Puz, N_Puz), em que Puz e um puzzle, N_Puz e o puzzle
resultante de inicializar o puzzle Puz.
----------------------------------------------------------------*/
inicializa(Puz, N_Puz) :-
    aplica_R1_R2_puzzle(Puz, T1),
    aplica_R1_R2_puzzle(T1, T2),
    (puzzles_iguais(T1, T2), N_Puz = T1, ! ; inicializa(T2, N_Puz), !).

/*----------------------------------------------------------------
verifica_R3(Puz): true se no puzzle Puz todas as linhas sao diferentes 
entre si e todas as colunas sao diferentes entre si.
----------------------------------------------------------------*/
aux_aux_verifica_R3(_, []):- !.

aux_aux_verifica_R3(L, [H|T]) :-
    \+listas_R3_iguais(L, H),
    aux_aux_verifica_R3(L, T), !.

aux_verifica_R3(Puz) :-
    head_tail(Puz, H, T),

    aux_aux_verifica_R3(H, T),
    (length(T, 1), true, !
    ; 
    aux_verifica_R3(T), !).

verifica_R3(Puz) :-
    aux_verifica_R3(Puz),
    mat_transposta(Puz, T),
    aux_verifica_R3(T).

/*----------------------------------------------------------------
propaga_posicoes(Posicoes, Puz, N_Puz), Posicoes e uma lista de
posicoes e Puz e um puzzle, N_Puz e o resultado de propagar, 
recursivamente, (as mudancas de) as posicoes de Posicoes.
----------------------------------------------------------------*/
propaga_posicoes(Posicoes, Puz, N_Puz) :-
    head_tail(Posicoes, H, T),
    get_coords(H, X, Y),
    get_linha(Puz, X, Linha),
    aplica_R1_R2_fila(Linha, N_Linha),

    (listas_iguais(Linha, N_Linha), Alteracoes1 = [], L_Puz = Puz, !
    ;
    mat_muda_linha(Puz, X, N_Linha, L_Puz), get_alteracoes(Puz, L_Puz, X, 1, Alteracoes1), !
    ),

    mat_elementos_coluna(L_Puz, Y , Coluna),
    aplica_R1_R2_fila(Coluna, N_Coluna),

    (listas_iguais(Coluna, N_Coluna), Alteracoes2 = [], C_Puz = L_Puz, !
    ;
    mat_muda_coluna(L_Puz, Y, N_Coluna, C_Puz), get_alteracoes(L_Puz, C_Puz, Y, 2, Alteracoes2), !),

    flatten([Alteracoes1, Alteracoes2, T], Res),

    (length(Res, 0), N_Puz = C_Puz, ! 
    ;
    propaga_posicoes(Res, C_Puz, N_Puz)).

/*----------------------------------------------------------------
resolve(Puz,Sol) o puzzle Sol e (um)a solucao do puzzle Puz.
----------------------------------------------------------------*/
aux_resolve(Puz, Coor, N_Puz) :-
    mat_muda_posicao(Puz, Coor, 0, T_Puz),
    (propaga_posicoes([Coor], T_Puz, TT_Puz),
        (verifica_R3(TT_Puz),
            mat_dimensoes(Puz, L, C),
            get_empty(TT_Puz, 1, L, C, [], Empty_Coors),
            (length(Empty_Coors, 0),
                N_Puz = TT_Puz, ! 
                ;
                head_tail(Empty_Coors, H, _),
                aux_resolve(TT_Puz, H, N_Puz), !
            ), !
            ;
            mat_muda_posicao(Puz, Coor, 1, X_Puz),
            (propaga_posicoes([Coor], X_Puz, XX_Puz),
                (verifica_R3(XX_Puz),
                    mat_dimensoes(Puz, L, C),
                    get_empty(XX_Puz, 1, L, C, [], Empty_Coord),
                    (length(Empty_Coord, 0), 
                        N_Puz = XX_Puz, !
                        ;
                        head_tail(Empty_Coord, H, _), aux_resolve(XX_Puz, H, N_Puz), !
                    ), !
                    ;
                    N_Puz = Puz, !
                ), !
                ;
                N_Puz = Puz, !
            ), !
        ), !
        ;
        (
            mat_muda_posicao(Puz, Coor, 1, X_Puz),
            propaga_posicoes([Coor], X_Puz, XX_Puz),
            (verifica_R3(XX_Puz),
                mat_dimensoes(Puz, L, C),
                get_empty(XX_Puz, 1, L, C, [], Empty_Coord),
                (length(Empty_Coord, 0),
                    N_Puz = XX_Puz, !
                    ;
                    head_tail(Empty_Coord, H, _),
                    aux_resolve(XX_Puz, H, N_Puz), !
                ), !
                ;
                N_Puz = Puz, !
            )
        ), !
    ).

resolve(Puz, Sol) :-
    inicializa(Puz, T_Puz),
    (verifica_R3(T_Puz), 
        true, ! 
        ; 
        !, false),
    mat_dimensoes(T_Puz, L, C),
    get_empty(T_Puz, 1, L, C, [], Empty_Coors),
    (length(Empty_Coors, 0), 
        Sol = T_Puz, !
        ;
        head_tail(Empty_Coors, H, _),
        aux_resolve(T_Puz, H, Sol), !).