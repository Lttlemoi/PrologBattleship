%% Battleship Solitaire
%% Idee 2: werken vanuit toegelaten omliggende cellen, per soort

%% Algorithm outline


%% prepend element to list
prepend(X, List, [X|List]).

%% append element to list
append(X, [], [X]).
append(X, [Head|List], [Head|XList]) :- append(X, List, XList).

%% create list of a certain length, filled with a certain element
fillList(X, 1, [X]).
fillList(X, N, [X|List]) :- fillList(X, M, List), N is 1+M.

%% nth0 equivalent for matrices
m_nth0(X, Y, LList, Elem) :- nth0(X, List, Elem),
							nth0(Y, LList, List).

%% surrounds the field with water
surroundRow(Row, SurRow) :- prepend('~', Row, SurRowTemp),
							append('~', SurRowTemp, SurRow),!.

surroundRows([],[]).
surroundRows([Head|Tail], [WaterHead|WaterTail]) :- surroundRow(Head, WaterHead),
													surroundRows(Tail, WaterTail),!.

surroundCols(Rows, SurRows) :- 	length(Rows, Width),
								fillList('~', Width, WaterRow),
								prepend(WaterRow, Rows, SurRowsTemp),
								append(WaterRow, SurRowsTemp, SurRows),!.

surroundBuffer(Field, WaterField) :- surroundCols(Field, WaterFieldTemp),
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

%% checks if cell has correct neighbours
correctNeighbours(X, Area) :-	neighbourRule(X, CorrectArea),
								Area = CorrectArea.

%% checks if all cells in a single row, surrounded by other rows above and below, are valid. Depends on buffer (water) surrounding area!
checkRow([[LU,U,RU],[L,C,R],[LL,D,RL]]) :- correctNeighbours(C, [[LU,U,RU],[L,C,R],[LL,D,RL]]).
checkRow([[LU,U,RU|TailU],[L,C,R|TailC],[LL,D,RL|TailL]]) :- 	correctNeighbours(C, [[LU,U,RU],[L,C,R],[LL,D,RL]]), 
																checkRow([[U,RU|TailU],[C,R|TailC],[D,RL|TailL]]).

%% checks if all rows in a list of rows are valid. Depends on buffer (water) surrounding area!
checkRows([Upper,Center,Lower]) :- checkRow([Upper,Center,Lower]).
checkRows([Upper,Center,Lower|Tail]) :- checkRow([Upper,Center,Lower]), checkRows([Center,Lower|Tail]).

%% WIP, incomplete
checkValidField(Field,BufferField) :- 	surroundBuffer(Field, BufferField),
										checkRows(BufferField).

%% counts the amount of ship parts are present in a single row
%% countRowShipElems([],0).
%% countRowShipElems([Head|Tail],N) :- shipPart(Head), countRowShipElems(Tail, M), N is 1+M;
%% 									not(shipPart(Head)), countRowShipElems(Tail, N).

%% countColShipElems([],0,_).

%% battleship(Field,Colnums,Rownums,Shipsizes,Field) :- 

