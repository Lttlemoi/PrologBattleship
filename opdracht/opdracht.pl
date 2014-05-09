#!/usr/bin/swipl -q -s

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
% Retry with a transposed map to get the columns right.
% Make sure no two ships touch eachother

%TEMP: working_directory(_, "C:\\Users\\User\\campus\\Programming paradigms\\Prolog\\PrologBattleship\\opdracht").

% Puts everything in a module and only exports the listed predicates
:- module(opdrachtBase, [testRow/4,
                         testRows/4,
                         testCol/3,
                         testCols/3,
                         testParts/5]).


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
isWater('~').

% Test if a row is valid and counts the length of the horizontal ships.
% A row is valid if
%     - horizontal ships have the right direction: w, x, x, ..., e
%     - only ship parts of the same ship are touching
%     - maximum 1 center part without surrounding w/e
% All center parts of horizontal ships are replaced with xh or xv, depending on the orientation of the ship
% testRow(input, lastItem, inShip?, output, shipSizes, amount of ship parts)

% base cases: all valid row ends
testRow([], o, false, [], [], 0).                                                       % base case: last element is a unicellular ship
testRow([], xv, false, [], [], 0).                                                      % base case: last element is a ship center not part of a horizontal ship
testRow([], e, false, [], [], 0).                                                       % base case: last element is the east end of a horizontal ship
testRow([], n, false, [], [], 0).                                                       % base case: last element is part of a vertical ship
testRow([], s, false, [], [], 0).                                                       % base case: last element is part of a vertical ship
testRow([], '~', false, [], [], 0).                                                     % base case: last element is not part of a ship
testRow([o|XS], '~', false, [o| KS], [1|LS], C) :- testRow(XS, o, false, KS, LS, CC),
                                                 C is CC+1,
                                                 C > 0.                             % unicellular ship can only be after a non-shippart
testRow([w|XS], '~', false, [w| KS], [L|LS], C) :- testRow(XS, w, true, KS, [LL|LS], CC),
                                                 C is CC+1,
                                                 C > 1,
                                                 L is LL+1,
                                                 L > 1.                             % beginning of multicellular ship
testRow([e|XS], w, true, [e| KS], [1|LS], C) :- testRow(XS, e, false, KS, LS, CC),
                                                C is CC+1,
                                                 C > 0.                              % ending of multicellular ship
testRow([e|XS], xh, true, [e| KS], [1|LS], C) :- testRow(XS, e, false, KS, LS, CC),
                                                C is CC+1,
                                                 C > 0.                              % ending of multicellular ship
testRow([x|XS], w, true, [xh| KS], [L|LS], C) :- testRow(XS, xh, true, KS, [LL|LS], CC),
                                                 C is CC+1,
                                                 C > 1,
                                                 L is LL+1,
                                                 L > 1.                             % middle of multicellular horizontal ship
testRow([x|XS], xh, true, [xh| KS], [L|LS], C) :- testRow(XS, xh, true, KS, [LL|LS], CC),
                                                 C is CC+1,
                                                 C > 1,
                                                 L is LL+1,
                                                 L > 1.                             % middle of multicellular horizontal ship
testRow([n|XS], '~', false, [n| KS], LS, C) :- testRow(XS, n, false, KS, LS, CC),
                                             C is CC+1,
                                                 C > 0.                                 % top of vertical multicellular ship
testRow([s|XS], '~', false, [s| KS], LS, C) :- testRow(XS, s, false, KS, LS, CC),
                                             C is CC+1,
                                                 C > 0.                                 % top of vertical multicellular ship
testRow([x|XS], '~', false, [xv| KS], LS, C) :- testRow(XS, xv, false, KS, LS, CC),
                                              C is CC+1,
                                                 C > 0.                                % middle of vertical multicellular ship
testRow(['~'|XS], _, false, ['~'| KS], LS, C) :- testRow(XS, '~', false, KS, LS, C).    % water right after something that is not in the middle of a horizontal ship

% shorthand version to test a row
testRow(In, Out, LCount, PCount) :- testRow(In, '~', false, Out, LCount, PCount).

% isShipEnd(n).
% isShipEnd(s).
% isShipEnd(e).
% isShipEnd(w).
% isShipEnd(xh).
% isShipEnd(o).
% isShipEnd('~').

% allowedPair(n, s).
% allowedPair(xv, s).
% allowedPair(n, xv).
% allowedPair('~', X) :- isShipEnd(X).
% allowedPair(X, '~') :- isShipEnd(X).

% testPairs([], []).
% testPairs([X|XS], [Y|YS]):- allowedPair(X, Y), testPairs(XS, YS).


% test all rows & count the length of the ships and number of parts per row
% testRows(In, Out, PartsPerRow, shipLengths)
testRows([], [], [], []).
testRows([X|XS], [Y|YS], [K|RC], LengthCount) :- testRow(X, Y, CC, K),
                                                 append(CC, LC, LengthCount),
                                                 testRows(XS, YS, RC, LC).

% testRows([X], [Y], [K], [CC]) :- testRow(X, Y, CC, K).
% testRows([X1, X2|XS], [Y1, Y2|YS], [K1, K2|RC], LengthCount) :- testRow(X1, Y1, CC1, K1),
%                                                                 testRow(X2, Y2, _, K2),
%                                                                 testPairs(Y1, Y2),
%                                                                 noTouch(X1, ['~'|X2]), noTouch(X2, ['~'|X1]),
%                                                                 append(CC1, LC, LengthCount),
%                                                                 testRows([X2|XS], [Y2|YS], [K2|RC], LC).



% Test if a columnn is valid and counts the length of the vertical ships. (including unicellular ships!)
% A column is valid if
%     - vertical ships have the right direction: n, x, x, ..., s
%     - only ship parts of the same ship are touching
%     - maximum 1 center part without surrounding w/e, center part has to be xh
% testCol(input, lastItem, inShip?, shipSizes)

% base cases: all valid column ends
testCol([], o, false, [], 0).                                               % base case: last element is a unicellular ship
testCol([], xh, false, [], 0).                                              % base case: last element is a ship center not part of a vertical ship
testCol([], s, false, [], 0).                                               % base case: last element is the east end of a vertical ship
testCol([], e, false, [], 0).                                               % base case: last element is part of a horizontal ship
testCol([], w, false, [], 0).                                               % base case: last element is part of a horizontal ship
testCol([], '~', false, [], 0).                                             % base case: last element is not part of a ship
testCol([o|XS], '~', false, [1|LS], C) :- testCol(XS, o, false, LS, CC),
                                        C is CC+1,
                                        C > 0.                          % unicellular ship can only be after a non-shippart
testCol([n|XS], '~', false, [L|LS], C) :- testCol(XS, n, true, [LL|LS], CC),
                                        C is CC+1,
                                        C > 1,
                                        L is LL+1,
                                        L > 1.                          % beginning of multicellular ship
testCol([s|XS], n, true, [1|LS], C) :- testCol(XS, s, false, LS, CC),
                                       C is CC+1,
                                        C > 0.                           % ending of multicellular ship
testCol([s|XS], xv, true, [1|LS], C) :- testCol(XS, s, false, LS, CC),
                                        C is CC+1,
                                        C > 0.                          % ending of multicellular ship
testCol([xv|XS], n, true, [L|LS], C) :- testCol(XS, xv, true, [LL|LS], CC),
                                        C is CC+1,
                                        C > 1,
                                        L is LL+1,
                                        L > 1.                          % middle of multicellular vertical ship
testCol([xv|XS], xv, true, [L|LS], C) :- testCol(XS, xv, true, [LL|LS], CC),
                                         C is CC+1,
                                        C > 1,
                                         L is LL+1,
                                        L > 1.                         % middle of multicellular vertical ship
testCol([w|XS], '~', false, LS, C) :- testCol(XS, w, false, LS, CC),
                                    C is CC+1,
                                        C > 0.                              % top of horizontal multicellular ship
testCol([e|XS], '~', false, LS, C) :- testCol(XS, e, false, LS, CC),
                                    C is CC+1,
                                        C > 0.                              % top of horizontal multicellular ship
testCol([xh|XS], '~', false, LS, C) :- testCol(XS, xh, false, LS, CC),
                                     C is CC+1,
                                        C > 0.                             % middle of horizontal multicellular ship
testCol(['~'|XS], _, false, LS, C) :- testCol(XS, '~', false, LS, C).       % water right after something that is not in the middle of a vertical ship

% shorthand version to test a column
testCol(In, LCount, PCount) :- testCol(In, '~', false, LCount, PCount).

% test all columns & count the length of the ships and number of parts per row
% testCols(In, PartsPerRow, shipLengths)
testCols([], [], []).
testCols([[]|XS], [], []):- testCols(XS, [], []).
% testCols([X|XS], [K|RC], LengthCount) :- testCol(X, CC, K),
testCols(XS, [K|RC], LengthCount) :- p_reapFirst(XS, YS, AS),
                                     testCol(AS, CC, K),
                                     append(CC, LC, LengthCount),
                                     % testCols(XS, RC, LC).
                                     testCols(YS, RC, LC).

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

% converts a list of ship lengths to the list format from the assignment.
% e.g. with ships of size [3, 1, 5, 2, 1, 1, 2, 1] -> [2, 2, 1, 0, 1]
% NOTE: the other predicates count the unicellular ships twice. This predicate fixes that.
% mergeLengths(In, Out)
mergeLengths([], [], _).
mergeLengths(XS, [L|LS], K) :- subtract(XS, [K], YS),
                               length(XS, LX),
                               LX > 0, length(YS, LY),
                               L is LX-LY,
                               KK is K+1,
                               mergeLengths(YS, LS, KK).
mergeLengths(XS, [L| LS]) :- mergeLengths(XS, [LL|LS], 1),
                             L is LL/2.

noTouch([],_).
noTouch(['~'|XS], [_|YS]):- noTouch(XS, YS).
noTouch([_|XS], ['~'|YS]):- noTouch(XS, YS).

noTouchRows([]).
noTouchRows([_]).
noTouchRows([XS, YS|XXS]) :- noTouch(XS, ['~'|YS]), noTouch(YS, ['~'|XS]), noTouchRows([YS|XXS]).


testParts(In, In, RowSums, ColSums, Lengths) :- testRows(In, Out, RowSums, RowLengths),
                                                noTouchRows(In),
                                                testCols(Out, ColSums, ColLengths),
                                                % transpose_m(Out, OutT),
                                                % testCols(OutT, ColSums, ColLengths),
                                                append(RowLengths, ColLengths, AllLengths),
                                                mergeLengths(AllLengths, Lengths), !.

