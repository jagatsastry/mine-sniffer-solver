/**
%Uncomment if nth0 is not-defined by the interpreter.
%This is not built-in to XSB
nth0(0,[X|_],X).
nth0(Idx,[_|List],X) :-
    Idx > 0,
    Idx1 is Idx-1,
    nth1(Idx1,List,X).
*/

%Check if [X, Y] is adjacent to [I, J] and has a Mine
adjMines(Grid, [I,J], [X,Y], M, N) :-
        hasMine(Grid, I - 1, J - 1, M, N), X is I - 1, Y is J - 1;
	hasMine(Grid, I - 1, J,     M, N), X is I - 1, Y is J;
	hasMine(Grid, I - 1, J + 1, M, N), X is I - 1, Y is J + 1;
	hasMine(Grid, I,     J - 1, M, N), X is I,     Y is J - 1;
	hasMine(Grid, I,     J + 1, M, N), X is I,     Y is J + 1;
	hasMine(Grid, I + 1, J - 1, M, N), X is I + 1, Y is J - 1;
	hasMine(Grid, I + 1, J,     M, N), X is I + 1, Y is J;
	hasMine(Grid, I + 1, J + 1, M, N), X is I + 1, Y is J + 1.

%Checks if cell [I, J] have a mine
hasMine(Grid, I, J, M, N) :-
	I >= 0, I < M,
	J >= 0, J < N,
	Idx is I * N + J,
	nth0(Idx, Grid, Cell),
	isMine(Cell).

%True if it is a mine
isMine(mine).

%True if the cell is not a number - either a mine or no_mine
nonnum_cell(mine).
nonnum_cell(no_mine).
%nonnum_cell(nh). %Equivalent to no_mine. Doesn't serve any purpose

solved(_, [], _, _, _).

solved(Grid, [Cell | Rest], Idx, M, N) :-
	nonnum_cell(Cell), %Cell is m0,
	Idx1 is Idx + 1,
	solved(Grid, Rest, Idx1, M, N);

	not(nonnum_cell(Cell)),
	I is Idx//N,
	J is Idx mod N,
	Idx1 is Idx + 1,
	findall([X, Y], adjMines(Grid, [I, J], [X, Y], M, N), AllMines),
	length(AllMines, Num),
	Cell is Num,
	solved(Grid, Rest, Idx1, M, N).

%Verify a filled grid
solve(Grid, [], _, M, N) :-
	solved(Grid, Grid, 0, M, N).

solve(Grid, [Cell | Rest], Idx, M, N) :-
        nonnum_cell(Cell),
	Idx1 is Idx + 1,
	solve(Grid, Rest, Idx1, M, N);

	not(nonnum_cell(Cell)),
	Idx1 is Idx + 1,
	solve(Grid, Rest, Idx1, M, N).

%Solves a flattened minesniffer grid
solve(Grid, M, N) :-
	solve(Grid, Grid, 0, M, N).

%Entry point. Pass a 2d grid like
%solve([[1, 1, Y, 0], [X, 1, 0, 0], [1, 1, Z, 0]]).
%Note: Different unknowns should have different Variable names
solve(Grid2d) :-
	length(Grid2d, M),
	nth0(0, Grid2d, Row),
	length(Row, N),
	flatten(Grid2d, FlatGrid),
	solve(FlatGrid, M, N).

array([_|_]).

unwrap(X, [X]) :-
	not(array(X)).

unwrap([X], Res) :-
	unwrap(X, Res).


flatten([Head|[]], Res) :-
	unwrap(Head, Res).

%Flatten a 2D list to a 1D list
flatten([Head|Tail], Flatlist) :-
	unwrap(Head, Head1),
	flatten(Tail, Rest),
	append(Head1, Rest, Flatlist).

/*
* ***********************
* Sample input and output
* ***********************
58 ?- solve([[1, 2, 2, 1], [1, X, Y, 1], [1, 2, 2, 1]]).
X = Y, Y = m1;
false.

74 ?- solve([[1, X], [1, Y]]).
X = mine,
Y = no_mine ;
X = no_mine,
Y = mine ;
false.

75 ?- solve([[1, 1, Y, 0], [X, 1, 0, 0], [1, 1, Z, 0]]).
Y = no_mine,
X = mine,
Z = no_mine ;
false.

**/














