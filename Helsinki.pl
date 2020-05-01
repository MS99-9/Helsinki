grid_build(N,M):-
    grid_build_helper(N, M,N).

grid_build_helper(_,[],0).
grid_build_helper(N, M,X):-
    X>0,
    M = [H|T],
    length(H,N),
    X1 is X-1,
    grid_build_helper(N,T,X1).

seq(X,Y,X) :- X=<Y.
seq(X,Y,Z) :- X<Y, X1 is X + 1,
seq(X1,Y,Z).

grid_gen(N,M):- grid_build(N,M), grid_gen1(N,M).

grid_gen1(_,[[]]).
grid_gen1(N,[[]|[H|T]]):-
    grid_gen1(N,[H|T]).

grid_gen1(N,[[H|T]|T1]):-
    seq(1,N,H),
    grid_gen1(N,[T|T1]).
    
num_gen(F,L,[]):- F>L.
num_gen(F,L,[F|T]):- 
    F=<L,
    F1 is F+1,
    num_gen(F1,L,T).
nlength([],0).
nlength([H|T],N):- 
			nlength(T,N1),
			N is N1+1.
			
check_num_grid(G):-

flatten(G,N),
max_list(N,Max),
nlength(G,S),
S>=Max,
checkhelper(N,1,Max).

checkhelper(_,Max,Max).
checkhelper(N,X,Max):- X<Max, 
			   mem(X,N),
                   X1 is X+1,
                   checkhelper(N,X1,Max).

mem(E,[E|_]).
mem(E,[X|T]):- X \= E,
mem(E,T).                   
trans([[]|_], []).

trans(G, [H|T]) :- 
    trans_helper(G, H, R),
	trans(R, T).

trans_helper([], [], []).

trans_helper([[H|T]|R], [H|H1], [T|T1]):- 
    trans_helper(R, H1, T1).

acceptable_distribution(G):- 
    trans(G, R),
    acceptable_distribution(G,R).

acceptable_distribution([],[]).
acceptable_distribution(G,R):- 
    G = [H|T],
	R =[H1|T1],
    H1 \= H,
    acceptable_distribution(T, T1).
            
distinct_rows([]).
distinct_rows(M):-
    M = [H|T],  
	distinct_rows(H,T),
	distinct_rows(T).
	
distinct_rows(_,[]).
distinct_rows(H, [H1|T]):-
    H1 \= H,
	distinct_rows(H,T).

distinct_columns(M):-
    trans(M,R),
    distinct_rows(R).

row_col_match(G):-
    trans(G,R),
    equality1(G,R).
equality1([],_).
equality1([G|T1],[R|T]):-
    
    equality2(G,[R|T]),equality1(T1,[R|T]).
equality2(H,[]):-
    member(2,[1]).
equality2(G,[R|T]):-
    G=R;
    equality2(G,T).

			

helsinki(N,G):- 
    grid_gen(N,G),
    check_num_grid(G),
    acceptable_distribution(G),
    distinct_rows(G),
    distinct_columns(G),
    row_col_match(G).

perm([H|T],L) :- perm(T,P), insert(H,P,L).
perm([],[]).
insert(X,[],[X]).
insert(X,[H|T],[X,H|T]).
insert(X,[H|T],[H|NT]):- insert(X,T,NT).
changed([],[]).
changed([H|T],[H1|T1]):- H\=H1,changed(T,T1).
acceptable_permutation(L,R):- perm(L,R),changed(L,R).

perm1([],[]).
perm1([H|T],[H1|T1]):-
H \= H1,
perm1(T,T1).
permuhelp(_,[]).
permuhelp(H,[H1|T1]):-
H\=H1,
permuhelp(H,T1).
permuhelp1([],[]).
permuhelp1([H|T],L):-
\+permuhelp(H,L),
permuhelp1(T,L).
acceptable_permutation2(R,L):-
R=[H|T],L=[H1|T1],
perm1(R,L),
permuhelp1(R,L).
