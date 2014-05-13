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
%:- module(opdrachtBase, []).

% testRow(thisRow, RowAbove, partLeft, partAbove, pCountRow, pCount)
%
%   t Y  YS
% l X XS
testRow([], [], '~', _, 0, []).
testRow([], [], o, '~', 0, []).
testRow([], [], n, '~', 0, []).
testRow([], [], s, x, 0, []).
testRow([], [], s, n, 0, []).
testRow([], [], w, '~', 0, []).
testRow([], [], x, '~', 0, []).
testRow([], [], x, '~', 0, []).
testRow([X|XS], [Y|YS], x, x, 0, []):- testRow(XS, YS, X, Y).

test(x).
test(y).
test2(X):- (X=x;X=y).
testAll([]).
testAll([X|XS]):- test(X), testAll(XS).
testAll2([]).
testAll2([X|XS]):- test2(X), testAll2(XS).

createList(0, []).
createList(K, [X|XS]):- K > 0,
                        KK is K-1,
                        createList(KK, XS),
                        random_member(X, [x,y]).