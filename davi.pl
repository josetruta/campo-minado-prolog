% Fatos - Emulando uma declaração de tipos.

cell(mine).
cell(int_0).
cell(int_1).
cell(int_2).
cell(int_3).
cell(int_4).
cell(int_5).
cell(int_6).
cell(int_7).
cell(int_8).

state(hidden).
state(revealed).
state(flagged).

difficulty(easy).
difficulty(medium).
difficulty(hard).

mode(classical).
mode(survival).
mode(timed).


main:-
    write("Dificuldade: "),
    read(Difficulty),
    getSize(Difficulty, Size),
    write("Quantidade de linhas: "),
    writeln(Size),
    generateMines(Size, ListMines),
    write("Coordenadas das minas: "),
    writeln(ListMines),
    generateBoard(Difficulty, Board),
    writeln("Board: "),
    writeln(Board),
    print_board(Board),
    writeln("Acao (dig ou flag): "),
    read(Action),
    writeln("Coord X: "),
    read(X),
    writeln("Coord Y: "),
    read(Y),
    updateBoard(X, Y, Action, Board, NewBoard),
    writeln(NewBoard),
    print_board(NewBoard).


% Regras




getSize(Diff, Size):-
    (   Diff = hard    ->
        (Size = 24);
        (   Diff = medium  ->
            (Size = 16);
            (Size = 8))).

getNumberOfMines(Size, NumMines):-
    NumMines is (Size * Size // 6).

formatListMines([],[],_):-!.
formatListMines([Mine|T1], [[X, Y]|T2], Size):-
    X is (Mine // Size) + 1,
    Y1 is Mine mod Size,
    (   Y1 =:= 0    ->
        (Y = Size);
        (Y = Y1)),
    formatListMines(T1, T2, Size). 

generateMines(Size, ListMines):-
    getNumberOfMines(Size, NumMines),
    TotalLength is Size * Size,
    randset(NumMines, TotalLength, Mines),
    formatListMines(Mines, ListMines, Size).

neighbors([X, Y], Neighbors) :-
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    Neighbors = [[X1, Y1], [X1, Y], [X1, Y2],
                [X,  Y1],           [X,  Y2],
                [X2, Y1], [X2, Y], [X2, Y2]].

countNeighborsMines([],_,0).
countNeighborsMines([[X, Y]|T], ListMines, C):-
    (   member([X, Y], ListMines) ->
        (	countNeighborsMines(T, ListMines, C1),
            C is C1+1);
        countNeighborsMines(T, ListMines, C)).

contains([X, Y], [[X, Y]|_]):-!.
contains(Coord, [_|Rest]):-
    contains(Coord, Rest).

getState([X, Y], ListMines, [Cell, State]):-
    (   contains([X, Y], ListMines)   ->
        (Cell = mine,
        State = hidden);
        (neighbors([X, Y], Neighbors),
        countNeighborsMines(Neighbors, ListMines, Cell),
        State = hidden)).

generateBoard(Diff, Board):-
    getSize(Diff, Size),
    generateMines(Size, ListMines),
    generateBoard(Size, Size, Board, ListMines).

generateBoard(0,_,[],_):-!.
generateBoard(NRow, MCol, [Row|Rest], ListMines):-
    generateBoardRow(NRow, MCol, Row, ListMines),
    NRow1 is NRow - 1,
    generateBoard(NRow1, MCol, Rest, ListMines).

generateBoardRow(_,0,[],_):-!.
generateBoardRow(IRow, ICol, [[Cell, State]|Rest], ListMines):-
    getState([IRow, ICol], ListMines, [Cell, State]),
    ICol1 is ICol - 1,
    generateBoardRow(IRow, ICol1, Rest, ListMines).



% Game Logic

digBoard(_, _, [], []).
digBoard(1, Y, [Row|Rest], [DiggedRow|Rest]):-
    digBoardRow(Y, Row, DiggedRow), !.
digBoard(X, Y, [Row|Rest], [Row|DiggedRest]):-
    X1 is X - 1,
    digBoard(X1, Y, Rest, DiggedRest).

digBoardRow(_, [], []).
digBoardRow(1, [[Cell, _]|Rest], [Digged|Rest]):-
    Digged = [Cell, revealed], !.
digBoardRow(Y, [Elem|Rest], [Elem|DiggedRest]):-
    Y1 is Y - 1,
    digBoardRow(Y1, Rest, DiggedRest).

flagBoard(_, _, [], []).
flagBoard(1, Y, [Row|Rest], [FlaggedRow|Rest]):-
    flagBoardRow(Y, Row, FlaggedRow), !.
flagBoard(X, Y, [Row|Rest], [Row|FlaggedRest]):-
    X1 is X - 1,
    flagBoard(X1, Y, Rest, FlaggedRest).

flagBoardRow(_, [], []).
flagBoardRow(1, [[Cell, _]|Rest], [Flagged|Rest]):-
    Flagged = [Cell, flagged], !.
flagBoardRow(Y, [Elem|Rest], [Elem|FlaggedRest]):-
    Y1 is Y - 1,
    flagBoardRow(Y1, Rest, FlaggedRest).

updateBoard(X, Y, dig, Board, UpdatedBoard):-
    digBoard(X, Y, Board, UpdatedBoard).
updateBoard(X, Y, flag, Board, UpdatedBoard):-
    flagBoard(X, Y, Board, UpdatedBoard).


/*Davi*/


print_board(Board) :-
    nl,
    print_column_numbers(Board),   
    print_separator(Board),       
    print_rows_with_numbers(Board, 1),  
    nl.


print_column_numbers(Board) :-
    length(Board, Size),  
    write('      '),      
    print_column_numbers_aux(1, Size),
    nl.

print_column_numbers_aux(Current, Max) :-
    Current =< Max,
    format('~|~`0t~d~2+', [Current]), 
    write('   '),  
    Next is Current + 1,
    print_column_numbers_aux(Next, Max).
print_column_numbers_aux(Current, Max) :-
    Current > Max.


print_separator(Board) :-
    length(Board, Size),  
    write('     '),       
    print_dashes(Size),
    nl.

print_dashes(0) :- !.
print_dashes(N) :-
    write('------'),  
    N1 is N - 1,
    print_dashes(N1).


print_rows_with_numbers([], _).
print_rows_with_numbers([Row|Rest], RowNum) :-
    format('~|~`0t~d~2+', [RowNum]),  
    write(' |   '),  
    print_row(Row),
    nl,
    NextRowNum is RowNum + 1,
    print_rows_with_numbers(Rest, NextRowNum).


print_row([]).
print_row([[Cell, State]|Rest]) :-
    print_cell(Cell, State),
    write('    '),  
    print_row(Rest).


print_cell(_, hidden) :-     
    write('\u25A0').              
print_cell(_, flagged) :-    
    write('\u25B8').              
print_cell(mine, revealed) :-  
    write('\u25CF').             
print_cell(Value, revealed) :-  
    Value \= mine,           
    write(Value).
