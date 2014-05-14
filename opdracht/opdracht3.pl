%% Stijn Manhaeve    20121442
%% Matthijs Van Os   20121014
%%
%% Opdracht Programming Paradigms 2013-2014
%% Prolog assignment:
%% Battleship Solitaire
%% Idee 2: werken vanuit toegelaten omliggende cellen, per soort

%% Algorithm outline:
%%  - Calculates all valid solutions, given the inital grid, according to the ruleset of the puzzle
%%  - Selects those solutions which have a correct amount of ship parts in all rows and columns
%%  - Selects the solutions which have the correct amount of ships of a certain length
%%  => These are the correct answers


%% create list of a certain length, filled with a certain element
fillList(_, 0, []).
fillList(X, N, [X|List]) :- N > 0, M is N-1, fillList(X, M, List).

%% nth0 equivalent for matrices
m_nth0(X, Y, LList, Elem) :- nth0(X, List, Elem),
                            nth0(Y, LList, List).

%% surrounds the field with water
surroundList(Row, Buffer, SurRow) :- append([Buffer|Row], [Buffer], SurRow),!.

surroundRows([],[]).
surroundRows([Head|Tail], [WaterHead|WaterTail]) :- surroundList(Head, '~', WaterHead),
                                                    surroundRows(Tail, WaterTail),!.

surroundCols([Row|Rows], SurRows) :-    length(Row, Width),
                                        fillList('~', Width, WaterRow),
                                        append([WaterRow|[Row|Rows]], [WaterRow], SurRows),!.

surroundBuffer(Field, WaterField) :-    surroundCols(Field, WaterFieldTemp),
                                        surroundRows(WaterFieldTemp, WaterField).


%% all elements of ships
shipPart(o).
shipPart(n).
shipPart(w).
shipPart(e).
shipPart(s).
shipPart(x).

%% '~' can have any neighbours (given the conditions of the following)
neighbourRule('~',[_,[_,'~',_],_]).

%% o can never have any neighbours
neighbourRule(o,[['~','~','~'],['~',o,'~'],['~','~','~']]).

%% n, e, s, and w have exactly one neighbour, which has to be either their complement or x
neighbourRule(n,[['~','~','~'],['~',n,'~'],['~',x,'~']]).
neighbourRule(n,[['~','~','~'],['~',n,'~'],['~',s,'~']]).
neighbourRule(w,[['~','~','~'],['~',w,x],['~','~','~']]).
neighbourRule(w,[['~','~','~'],['~',w,e],['~','~','~']]).
neighbourRule(e,[['~','~','~'],[x,e,'~'],['~','~','~']]).
neighbourRule(e,[['~','~','~'],[w,e,'~'],['~','~','~']]).
neighbourRule(s,[['~',x,'~'],['~',s,'~'],['~','~','~']]).
neighbourRule(s,[['~',n,'~'],['~',s,'~'],['~','~','~']]).

%% x has exactly two neighbours, either horizontally or vertically
neighbourRule(x,[['~',n,'~'],['~',x,'~'],['~',s,'~']]).
neighbourRule(x,[['~',x,'~'],['~',x,'~'],['~',s,'~']]).
neighbourRule(x,[['~',n,'~'],['~',x,'~'],['~',x,'~']]).
neighbourRule(x,[['~',x,'~'],['~',x,'~'],['~',x,'~']]).
neighbourRule(x,[['~','~','~'],[w,x,x],['~','~','~']]).
neighbourRule(x,[['~','~','~'],[x,x,x],['~','~','~']]).
neighbourRule(x,[['~','~','~'],[x,x,e],['~','~','~']]).
neighbourRule(x,[['~','~','~'],[w,x,e],['~','~','~']]).

%% checks if all cells in a single row, surrounded by other rows above and below, are valid. Depends on buffer (water) surrounding area!
checkRow([[LU,U,RU],[L,C,R],[LL,D,RL]]) :- neighbourRule(C, [[LU,U,RU],[L,C,R],[LL,D,RL]]).
checkRow([[LU,U,RU|TailU],[L,C,R|TailC],[LL,D,RL|TailL]]) :-    neighbourRule(C, [[LU,U,RU],[L,C,R],[LL,D,RL]]), 
                                                                checkRow([[U,RU|TailU],[C,R|TailC],[D,RL|TailL]]).

%% checks if all rows in a list of rows are valid. Depends on buffer (water) surrounding area!
checkRows([Upper,Center,Lower]) :- checkRow([Upper,Center,Lower]).
checkRows([Upper,Center,Lower|Tail]) :- checkRow([Upper,Center,Lower]), checkRows([Center,Lower|Tail]).

%% counts the amount of ship parts are present in a single row
countRowShipElems([], 0).
countRowShipElems(['~'|Tail],N):- !, countRowShipElems(Tail, N).
countRowShipElems([_|Tail],N) :- countRowShipElems(Tail, M), N is M+1.

%% counts the amount of ship parts in all rows of a field
countRows([Row], [N]) :- countRowShipElems(Row, N).
countRows([Row|Tail], [N|Counts]) :-    countRows(Tail, Counts),
                                        countRowShipElems(Row, N),!.

%% counts the amount of ship parts in a single column
countColShipElems([], _, 0).
countColShipElems([Row|Rows], N, ShipAmount) :- nth0(N, Row, Cell), 
                                                countColShipElems(Rows, N, M),
                                                ( shipPart(Cell) -> ShipAmount is M+1 ; ShipAmount is M ).

countCols(Rows, Counts) :-  length(Rows, Length),
                            Height is Length-1,
                            countAllCols(Rows, Height, Counts).

countAllCols(Rows, 0, [N]) :- countColShipElems(Rows, 0, N),!.
countAllCols(Rows, ColNum, NewCounts) :-    Prev is ColNum - 1,
                                            countAllCols(Rows, Prev, Counts),
                                            countColShipElems(Rows, ColNum, N),
                                            append(Counts, [N], NewCounts).


%% converts a list of ship lengths to the list format from the assignment.
%% e.g. with ships of size [3, 1, 5, 2, 1, 1, 2, 1] -> [2, 2, 1, 0, 1]
%% NOTE: the other predicates count the unicellular ships twice. This predicate fixes that.
%% mergeLengths(In, Out)
mergeLengths([], [], _).
mergeLengths(XS, [L|LS], K) :- subtract(XS, [K], YS),
                               length(XS, LX),
                               LX > 0, length(YS, LY),
                               L is LX-LY,
                               KK is K+1,
                               mergeLengths(YS, LS, KK).
mergeLengths(XS, [L| LS]) :- mergeLengths(XS, [LL|LS], 1),
                             L is LL/2.

countShip([], _, 0).
countShip([Cell|Row], End, Size) :- (   Cell = x ->
                                            countShip(Row, End, PrevSize),
                                            Size is PrevSize + 1,!
                                    ;   Cell = End ->
                                            Size is 2,!
                                    ;   write('SOMETHING WENT VERY WRONG\n'),
                                        true
                                    ).

countRowShips([], _, _, _).
countRowShips([Cell|Row], Start, End, Ships) :-    (   Cell = o ->
                                                        append(PrevShips, [1], Ships),
                                                        countRowShips(Row, Start, End, PrevShips),!
                                                    ;   Cell = Start ->
                                                            countShip(Row, End, Size),
                                                            append(PrevShips, [Size], Ships),
                                                            countRowShips(Row, Start, End, PrevShips),!
                                                    ;   countRowShips(Row, Start, End, Ships)
                                                    ).

countHorizontalShips([], []).
countHorizontalShips([Row|Rows], Ships) :-  countRowShips(Row, w, e, RowShips),
                                            countHorizontalShips(Rows, PrevShips),
                                            append(PrevShips, RowShips, Ships).


%% p_reapFirst(In, Rest, Out)
%% succeeds when In is a matrix, Out is the first column of that matrix (as a flat list) and Rest is the matrix without the first column.
p_reapFirst([], [], []).
p_reapFirst([[X|XS]|XXS], [XS|YYS], [X|AS]) :- p_reapFirst(XXS, YYS, AS).

countVShips([[]|_], First, First).
countVShips(Rows, Ships, First) :- p_reapFirst(Rows, Rest, Col),  
							countRowShips(Col, n, s, ColShips),
                            countVShips(Rest, PrevShips, First),
                            append(PrevShips, ColShips, Ships).

countAllShips(Field, Ships) :-  countHorizontalShips(Field, RowShips),
                                countVShips(Field, TotalShips, RowShips),
                                %append(RowShips, ColShips, TotalShips),
                                mergeLengths(TotalShips, Ships).


printList([]).  
printList([X|List]) :-  write(X), write(' '), printList(List).
printField([]).
printField([Head|Rows]) :- printList(Head), nl, printField(Rows).


%% wip incomplete
battleship(Field, RowCount, ColCount, ShipCount, BufferField) :-    surroundBuffer(Field, BufferField),
                                                                    checkRows(BufferField),
                                                                    countRows(Field, RowCount),
                                                                    countCols(Field, ColCount),
                                                                    countAllShips(Field, ShipCount),
                                                                    write('SOLUTION: \n'),
                                                                    printField(Field),!.
                                                                    
doTest(T, ExecTime) :- statistics(walltime, [_ | [_]]),
			T,
			statistics(walltime, [_ | [ExecTime]]),
			write('Execution took '),write(ExecTime), write(' ms.'), nl.