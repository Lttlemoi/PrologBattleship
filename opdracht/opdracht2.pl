
%%  Stijn Manhaeve    20121442
%%  Matthijs Van Os   20121014

%% ==========================================
%%  Opdracht Programming Paradigms 2013-2014
%%  Prolog assignment: BATTLESHIP SOLITAIRE
%% ==========================================


%% Algorithm outline:
%% \-> Calculates all valid solutions, given the inital grid, according to the ruleset of the puzzle (all cells have correct neighbours)
%%  \-> Selects those solutions which have a correct amount of ship parts in all rows and columns
%%   \-> Selects a solutions which has the correct amount of ships of a certain length


%% create list of a certain length, filled with a certain element
fillList(_, 0, []).
fillList(X, N, [X|List]) :- N > 0, M is N-1, fillList(X, M, List).

%% nth0 equivalent for matrices
m_nth0(X, Y, LList, Elem) :- nth0(X, List, Elem),
                            nth0(Y, LList, List).

p_reapFirst([], [], []).
p_reapFirst([[X|XS]|XXS], [XS|YYS], [X|AS]) :- p_reapFirst(XXS, YYS, AS).

%% transposes the given matrix
%% transpose_m(In, Out)
%% succesful if In and Out are each other's transposed forms.
%% will fail if the matrix is jagged.
transpose_m([], []).
transpose_m([[]|XS], []) :- transpose_m(XS, []).
transpose_m(XS, [AS|AAS]) :- p_reapFirst(XS, YS, AS),
                             transpose_m(YS, AAS).

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
%% these neighbours can be either another x, or a fitting edge piece (n, e, s, w)
neighbourRule(x,[['~',n,'~'],['~',x,'~'],['~',s,'~']]).
neighbourRule(x,[['~',x,'~'],['~',x,'~'],['~',s,'~']]).
neighbourRule(x,[['~',n,'~'],['~',x,'~'],['~',x,'~']]).
neighbourRule(x,[['~',x,'~'],['~',x,'~'],['~',x,'~']]).
neighbourRule(x,[['~','~','~'],[w,x,x],['~','~','~']]).
neighbourRule(x,[['~','~','~'],[x,x,x],['~','~','~']]).
neighbourRule(x,[['~','~','~'],[x,x,e],['~','~','~']]).
neighbourRule(x,[['~','~','~'],[w,x,e],['~','~','~']]).

%% checks if all cells in a single row, surrounded by other rows above and below, are valid
%% depends on buffer (water) surrounding area!
checkRow([[LU,U,RU],[L,C,R],[LL,D,RL]]) :- neighbourRule(C, [[LU,U,RU],[L,C,R],[LL,D,RL]]).
checkRow([[LU,U,RU|TailU],[L,C,R|TailC],[LL,D,RL|TailL]]) :-    neighbourRule(C, [[LU,U,RU],[L,C,R],[LL,D,RL]]), 
                                                                checkRow([[U,RU|TailU],[C,R|TailC],[D,RL|TailL]]).

%% checks if all rows in a list of rows are valid
%% depends on buffer (water) surrounding area!
checkRows([Upper,Center,Lower]) :- checkRow([Upper,Center,Lower]).
checkRows([Upper,Center,Lower|Tail]) :- checkRow([Upper,Center,Lower]), checkRows([Center,Lower|Tail]).


%% counts the amount of ship parts present in a single row
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

%% counts the amount of ship parts in all columns of a field
countCols(Rows, Counts) :-  length(Rows, Length),
                            Height is Length-1,
                            countAllCols(Rows, Height, Counts).

countAllCols(Rows, 0, [N]) :- countColShipElems(Rows, 0, N),!.
countAllCols(Rows, ColNum, NewCounts) :-    Prev is ColNum - 1,
                                            countAllCols(Rows, Prev, Counts),
                                            countColShipElems(Rows, ColNum, N),
                                            append(Counts, [N], NewCounts).


%% converts a list of ship lengths to the list format from the assignment
%% e.g. with ships of size [3, 1, 5, 2, 1, 1, 2, 1] -> [2, 2, 1, 0, 1]
%% NOTE: the other predicates count the unicellular ships twice. This predicate fixes that
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

%% counts towards the end of a ship
countShip([], _, 0).
countShip([Cell|Row], End, Size) :- (   Cell = x ->
                                            countShip(Row, End, PrevSize),
                                            Size is PrevSize + 1,!
                                    ;   Cell = End ->
                                            Size is 2,!
                                    ;   write('SOMETHING WENT VERY WRONG\n'),
                                        true
                                    ).

%% counts all ships in a list of cells
countRowShips([], _, _, _).
countRowShips([Cell|Row], Start, End, Ships) :- (   Cell = o ->
                                                        append(PrevShips, [1], Ships),
                                                        countRowShips(Row, Start, End, PrevShips),!
                                                ;   Cell = Start ->
                                                        countShip(Row, End, Size),
                                                        append(PrevShips, [Size], Ships),
                                                        countRowShips(Row, Start, End, PrevShips),!
                                                ;   countRowShips(Row, Start, End, Ships)
                                                ).

%% counts all horizontal ships (from 'w' to 'e') and small ships ('o') in a field
countHorizontalShips([], []).
countHorizontalShips([Row|Rows], Ships) :-  countRowShips(Row, w, e, RowShips),
                                            countHorizontalShips(Rows, PrevShips),
                                            append(PrevShips, RowShips, Ships).

%% counts all vertical ships (from 'n' to 's') and small ships ('o') in a field
countVerticalShips(Rows, Ships) :-  transpose_m(Rows, Cols),
                                    countVShips(Cols, Ships).

countVShips([], []).
countVShips([Col|Cols], Ships) :-   countRowShips(Col, n, s, ColShips),
                                    countVShips(Cols, PrevShips),
                                    append(PrevShips, ColShips, Ships).                                           

%% counts all ships on a field, both horizontally and vertically
countAllShips(Field, Ships) :-  countHorizontalShips(Field, RowShips),
                                countVerticalShips(Field, ColShips),
                                append(RowShips, ColShips, TotalShips),
                                mergeLengths(TotalShips, Ships).


%% print a list of atoms
printList([]).  
printList([X|List]) :-  write(X), write(' '), printList(List).

%% print an entire field
printField([]).
printField([Head|Rows]) :- printList(Head), nl, printField(Rows).


%% calculate a solution for the puzzle
battleship(Field, RowCount, ColCount, ShipCount, BufferField) :-    surroundBuffer(Field, BufferField),

                                                                    checkRows(BufferField),
                                                                    countRows(Field, RC), RC == RowCount,
                                                                    countCols(Field, CC), CC == ColCount,
                                                                    countAllShips(Field, Ships), Ships == ShipCount,

                                                                    write('SOLUTION: \n'),
                                                                    printField(Field),!.
