% Stijn Manhaeve    20121442
% Matthijs Van Os   20121014
%
% opdracht Programming Paradigms 2013-2014
% Prolog assignment:
%% Battleship Solitaire
%% Idee 2: werken vanuit toegelaten omliggende cellen, per soort

%% Algorithm outline


%% prepend element to list

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

%% wip incomplete
checkValidField(Field, RowCount, ColCount, BufferField) :-  surroundBuffer(Field, BufferField),
                                                            surroundList(RowCount, 0, BufRC),
                                                            surroundList(ColCount, 0, BufCC),
                                                            checkRows(BufferField),
                                                            printField(BufferField),
                                                            countRows(BufferField, RC),
                                                            countCols(BufferField, CC),
                                                            write('COMPARE R:\n\t'),write(RC),nl,write('\t'),write(BufRC),nl, RC == BufRC, 
                                                            write('COMPARE C:\n\t'),write(CC),nl,write('\t'),write(BufCC),nl, CC == BufCC,
                                                            write('SUCCESS\n'),!.

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

printList([]).  
printList([X|List]) :-  write(X), write(' '), printList(List).
printField([]).
printField([Head|Rows]) :- printList(Head), nl, printField(Rows).

%battleship(Field,Colnums,Rownums,Shipsizes,Field) :- 
