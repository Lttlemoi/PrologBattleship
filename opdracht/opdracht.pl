% Stijn Manhaeve	20121442
% Matthijs Van Os	20121014
%
% opdracht Programming Paradigms 2013-2014
% Prolog assignment:
% Battleship Puzzle

% Solving algorithm outline:
% Try to find a configuration such that:
%     -  ship part count on each row is correct
%     -  horizontal ships don't break the following constraints:
%            * part orientation
%            * neighboring parts are a valid ship
% Retry with a transposed map to get the collumns right.
% Make sure no two ships touch eachother
% General constraints:
%     - no two ships touch eachother
%     - ship count is correct

% Puts everything in a module and only exports the listed predicates
:- module(opdrachtBase, [getBoardSize/2,
                         boardConstraints/3,
                         countConstraints/3,
                         isShipPart/1,
                         countRows/2,
                         testRow/3,
                         transpose_m/2]).

p_getBoardSize([XS], Y) :- length(XS, Y).
p_getBoardSize([X|XS], Y) :- p_getBoardSize(XS, YY),
                             length(X, Y),
                             Y =:= YY.
% Get the size of the board. Succesful if all the rows have the exact same size
getBoardSize(XS, [X, Y]) :- p_getBoardSize(XS, Y),
                            length(XS, X).

% Test if the board dimensions are matched by the expected row & collumn counts
boardConstraints(Board, RowSums, CollumnSums) :- getBoardSize(Board, [RowC, ColC]),
                                                 length(RowSums, RowC),
                                                 length(CollumnSums, ColC).

p_sum([], 0).
p_sum([X|XS], K) :- p_sum(XS, KK),
                    K is KK + X.

p_mulsum([], _, 0).
p_mulsum([X|XS], N, K) :- p_mulsum(XS, N+1, KK),
                          K is KK + N*X.

% Test if all the ship part counts are correct. The total amount of ship parts counted by row, collumn or ship should all be the same.
countConstraints(RowSums, CollumnSums, ShipSums) :- p_sum(RowSums, Count),
                                                    p_sum(CollumnSums, Count),
                                                    p_mulsum(ShipSums, 1, Count).

% Convenience predicate to test if a cell is part of a ship, horizontal ship or vertical ship
isShipPart(n).
isShipPart(s).
isShipPart(e).
isShipPart(w).
isShipPart(x).
isShipPart(xh).
isShipPart(xv).
isShipPart(o).
% Needs to be explicitely specified what literals are not ship parts
not(isShipPart(k)).
%not(isShipPart("-")).

isHShipPart(e).
isHShipPart(w).
isHShipPart(xh).
isVShipPart(n).
isVShipPart(s).
isVShipPart(xv).

p_countRow([], 0).
p_countRow([X|XS], K) :- isShipPart(X),
                         p_countRow(XS, KK),
                         K is KK + 1.
p_countRow([X|XS], K) :- not(isShipPart(X)),
                         p_countRow(XS, K).

% Counts the ship parts on each row and puts the results in a list
countRows([], []).
countRows([X|XS], [K|KS]) :- p_countRow(X, K),
                             countRows(XS, KS).

% Test if a row is valid and counts the length of the horizontal ships.
% A row is valid if
%     - horizontal ships have the right direction: w, x, x, ..., e
%     - only ship parts of the same ship are touching
%     - maximum 1 center part without surrounding w/e
% All center parts of horizontal ships are replaced with xh
% testRow(input, lastItem, inShip?, output, shipSizes)

% base cases: all valid row ends
testRow([], o, false, [], []).                                                     % base case: last element is a unicellular ship
testRow([], x, false, [], []).                                                      % base case: last element is a ship center not part of a horizontal ship
testRow([], e, false, [], []).                                                     % base case: last element is the east end of a horizontal ship
testRow([], X, false, [], []) :- not(isShipPart(X)).                                % base case: last element is not part of a ship
testRow([o|XS], X, false, [o| KS], [1|LS]) :- not(isShipPart(X)),
                                              testRow(XS, o, false, KS, LS).        % unicellular ship can only be after a non-shippart
testRow([w|XS], X, false, [w| KS], [L|LS]) :- not(isShipPart(X)),
                                              testRow(XS, w, true, KS, [LL|LS]),
                                              L is LL+1.                            % beginning of multicellular ship
testRow([e|XS], w, true, [e| KS], [1|LS]) :- testRow(XS, e, false, KS, LS).         % ending of multicellular ship
testRow([e|XS], x, true, [e| KS], [1|LS]) :- testRow(XS, e, false, KS, LS).         % ending of multicellular ship
testRow([x|XS], w, true, [xh| KS], [L|LS]) :- testRow(XS, x, true, KS, [LL|LS]),
                                              L is LL+1.                            % middle of multicellular horizontal ship
testRow([x|XS], x, true, [xh| KS], [L|LS]) :- testRow(XS, x, true, KS, [LL|LS]),
                                              L is LL+1.                            % middle of multicellular horizontal ship
testRow([n|XS], X, false, [n| KS], LS) :- not(isShipPart(X)),
                                          testRow(XS, n, false, KS, LS).            % top of vertical multicellular ship
testRow([s|XS], X, false, [s| KS], LS) :- not(isShipPart(X)),
                                          testRow(XS, s, false, KS, LS).            % top of vertical multicellular ship
testRow([x|XS], X, false, [xv| KS], LS) :- not(isShipPart(X)),
                                           testRow(XS, x, false, KS, LS).           % middle of vertical multicellular ship
testRow([X|XS], _, false, [X| KS], LS) :- not(isShipPart(X)),
                                          testRow(XS, X, false, KS, LS).            % water right after something that is not in the middle of a horizontal ship

% shorthand version to test a row
testRow(In, Out, Count) :- testRow(In, k, false, Out, Count).


p_reapFirst([], [], []).
p_reapFirst([[X|XS]|XXS], [XS|YYS], [X|AS]) :- p_reapFirst(XXS, YYS, AS).

% transposes the given matrix
% transpose_m(In, Out)
% succesful if In and Out are eachothers transposed forms.
% will fail if the matrix is jagged.
transpose_m([], []).
transpose_m([[]|XS], []) :- transpose_m(XS, []).
transpose_m(XS, [AS|AAS]) :- p_reapFirst(XS, YS, AS),
                             transpose_m(YS, AAS).