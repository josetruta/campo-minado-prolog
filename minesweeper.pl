% Minesweeper


getSize(hard, 24).
getSize(medium, 16).
getSize(_, 8).


getTimeLimit(hard, 600).
getTimeLimit(medium, 400).
getTimeLimit(_, 200).


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
    (   X =< 0; Y =< 0   ),
    countNeighborsMines(T, ListMines, C).
countNeighborsMines([[X, Y]|T], ListMines, C):-
    (   member([X, Y], ListMines) ->
        (	countNeighborsMines(T, ListMines, C1),
            C is C1+1);
        countNeighborsMines(T, ListMines, C)).


getState([X, Y], ListMines, [Cell, State]):-
    (   member([X, Y], ListMines)   ->
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


digBoard(_, _, [], [], _).
digBoard(1, Y, [Row|Rest], [DiggedRow|Rest], [Cell, State]):-
    digBoardRow(Y, Row, DiggedRow, [Cell, State]), !.
digBoard(X, Y, [Row|Rest], [Row|DiggedRest], [Cell, State]):-
    X1 is X - 1,
    digBoard(X1, Y, Rest, DiggedRest, [Cell, State]).

digBoardRow(_, [], [], _).
digBoardRow(1, [[Cell, State]|Rest], [Digged|Rest], [Cell, State]):-
    Digged = [Cell, revealed], !.
digBoardRow(Y, [Elem|Rest], [Elem|DiggedRest], [Cell, State]):-
    Y1 is Y - 1,
    digBoardRow(Y1, Rest, DiggedRest, [Cell, State]).


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


updateBoard([], _, _, Board, Board):-!.
updateBoard([[X, Y]|Rest], dig, Size, Board, UpdatedBoard):-
    (   X =< 0; Y =< 0; X > Size; Y > Size	),
    updateBoard(Rest, dig, Size, Board, UpdatedBoard).
updateBoard([[X, Y]|Rest], dig, Size, Board, FinalBoard):-
    digBoard(X, Y, Board, TempBoard, [Cell, State]),
    (   (Cell = 0, State = hidden)  ->
        (   neighbors([X, Y], Neighbors),
            append(Rest, Neighbors, NewRest),
            updateBoard(NewRest, dig, Size, TempBoard, FinalBoard));
        (   (Cell = 0, State = flagged)  ->
            (   neighbors([X, Y], Neighbors),
                append(Rest, Neighbors, NewRest),
                updateBoard(NewRest, dig, Size, TempBoard, FinalBoard));
            (   updateBoard(Rest, dig, Size, TempBoard, FinalBoard)))).
updateBoard([[X, Y]], flag, _, Board, UpdatedBoard):-
    flagBoard(X, Y, Board, UpdatedBoard).


flattenBoard([Row|[]], Row).
flattenBoard([Row|Rest], Flatten):-
    flattenBoard(Rest, TempFlatten),
    append(Row, TempFlatten, Flatten).


checkDefeat(timed, Board, Difficulty, StartTime):-
    getTimeLimit(Difficulty, TimeLimit),
    durationTime(StartTime, Duration),
    (   Duration > TimeLimit    ->
        (   nl,
            writeln("  >> TEMPO ACABOU!"));
        (   checkDefeat(classic, Board, Difficulty, StartTime))).
checkDefeat(survival, [[Cell, flagged]|Rest], Difficulty, StartTime):-
    (   Cell = mine		->  
    	(   checkDefeat(survival, Rest, Difficulty, StartTime));
    	(   !)).
checkDefeat(_, [[mine, revealed]|_], _, _):-!.
checkDefeat(Mode, [_|Rest], Difficulty, StartTime):- checkDefeat(Mode, Rest, Difficulty, StartTime).


checkVictory([], wins).
checkVictory([[mine, _]|Rest], Status):- checkVictory(Rest, Status).
checkVictory([[_, hidden]|_], continues):- !.
checkVictory([[_, flagged]|_], continues):- !.
checkVictory([[_, _]|Rest], Status):- checkVictory(Rest, Status).


% Game Interface


printBoard(Board) :-
    nl,
    printColumnNumbers(Board),   
    printSeparator(Board),       
    printRowsWithNumbers(Board, 1),  
    nl.


printColumnNumbers(Board) :-
    length(Board, Size),  
    write('      '),      
    printColumnNumbersAux(1, Size),
    nl.


printColumnNumbersAux(Current, Max) :-
    Current =< Max,
    format('~|~`0t~d~2+', [Current]), 
    write(' '),  
    Next is Current + 1,
    printColumnNumbersAux(Next, Max).
printColumnNumbersAux(Current, Max) :-
    Current > Max.


printSeparator(Board) :-
    length(Board, Size),  
    write('     '),       
    printDashes(Size),
    nl.


printDashes(0) :- !.
printDashes(N) :-
    write('------'),  
    N1 is N - 2,
    printDashes(N1).


printRowsWithNumbers([], _).
printRowsWithNumbers([Row|Rest], RowNum) :-
    format('~|~`0t~d~2+', [RowNum]),  
    write(' |   '),  
    printRow(Row),
    nl,
    NextRowNum is RowNum + 1,
    printRowsWithNumbers(Rest, NextRowNum).


printRow([]).
printRow([[Cell, State]|Rest]) :-
    printCell(Cell, State),
    write('  '),  
    printRow(Rest).


printCell(_, hidden) :-     
    write('\u25A0').              
printCell(_, flagged) :-    
    write('\u25B8').              
printCell(mine, revealed) :-  
    write('\u25CF').             
printCell(Value, revealed) :-  
    Value \= mine,           
    write(Value).


action("D", dig).
action("B", flag).
action(_, dig).


getUserAction(Action, X, Y):-
    writeln("Escolha um Movimento: (D) Desenterrar ou (B) Bandeira, seguido de coordenadas (X, Y) (ex: D 1 1): "),
    read_line_to_string(user_input, Input),
    split_string(Input, " ", "", InputList),
    [ActionString, XString, YString] = InputList,
    action(ActionString, Action),
    atom_number(XString, X),
    atom_number(YString, Y).


% Timer


startTime(StartTime):- 
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, date(_, _, _, H, M, S, _, _, _), 'UTC'),
    StartTime is (H * 3600 + M * 60 + S).


durationTime(StartTime, Duration):-
    get_time(CurrentTimeStamp),
    stamp_date_time(CurrentTimeStamp, date(_, _, _, H, M, S, _, _, _), 'UTC'),
    CurrentTime is (H * 3600 + M * 60 + S),
    Duration is CurrentTime - StartTime.


printDurationTime(StartTime):-
    durationTime(StartTime, Duration),
    nl,
    write(" >> Tempo decorrido: "),
    DurationRounded is round(Duration),
    write(DurationRounded),
    writeln(" segundos."),
    nl.


% Text Cards


initCard:-
    format('~n'),
    format('\u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 ~n'),
    format('\u25A0                                     \u25A0~n'),
    format('\u25A0      C A M P O   M I N A D O        \u25A0~n'),
    format('\u25A0                                     \u25A0~n'),
    format('\u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 ~n'),
    format('~n').
   
loseCard:-
    format('~n'),
    format('\u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 ~n'),
    format('\u25A0                                     \u25A0~n'),
    format('\u25A0              PERDEU!!!              \u25A0~n'),
    format('\u25A0                                     \u25A0~n'),
    format('\u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 ~n'),
    format('~n').

winCard:-
    format('~n'),
    format('\u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 ~n'),
    format('\u25A0                                     \u25A0~n'),
    format('\u25A0              GANHOU!!!              \u25A0~n'),
    format('\u25A0                                     \u25A0~n'),
    format('\u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 \u25A0 ~n'),
    format('~n').


% Main


mode("C", classic).
mode("S", survival).
mode("T", timed).
%mode(_, classic).


difficulty("F", easy).
difficulty("M", medium).
difficulty("D", hard).
%difficulty(_, easy).


startGame(Mode, Difficulty):-
    mode(Mode, ChosenMode),
    difficulty(Difficulty, ChosenDifficulty),
    generateBoard(ChosenDifficulty, Board),
    printBoard(Board),
    startTime(StartTime),
    gameLoop(ChosenMode, ChosenDifficulty, Board, StartTime),
    printDurationTime(StartTime).


gameLoop(Mode, Difficulty, Board, StartTime):-
    getUserAction(Action, X, Y),
    getSize(Difficulty, Size),    
    updateBoard([[X, Y]], Action, Size, Board, UpdatedBoard),
    printBoard(UpdatedBoard),
    flattenBoard(UpdatedBoard, FlattenBoard),
    (   checkDefeat(Mode, FlattenBoard, Difficulty, StartTime)     ->
            (   loseCard);
            (   checkVictory(FlattenBoard, Status),
                (   Status = wins   ->
                    ( winCard);
                    gameLoop(Mode, Difficulty, UpdatedBoard, StartTime)))).


readMode(Mode):- read_line_to_string(user_input, Mode).
readDifficulty(Diff):- read_line_to_string(user_input, Diff).


main:-
    initCard,
    writeln("Escolha o modo de jogo - (C) Classico, (S) Survival, (T) Contra o Tempo: "),
    readMode(Mode),
    writeln("Escolha a dificuldade - (F) Facil, (M) Medio, (D) Dificil: "),
    readDifficulty(Difficulty),
    startGame(Mode, Difficulty),
    halt.