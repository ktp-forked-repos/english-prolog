% This is a file called 'general.pl' because it contains generally used functions available in their own module...
:- module('general',[
set_if_equal/4,if_then/2,if_then_/2,if_then_else/3,if_then_else_/3,if_else_/2,stuff/1,stuffn/1,stuffl/1,stuffa/1,bool_not/2,append/4, info/1,info/2]).
:- use_module('my_debug.pl').

% Sets C to the value of D, when A is equal to B.
% It always returns true.
set_if_equal(A, B, C, D):- A=B, (C is D); !, stuff(C,D).

% Does T, when I is equal to 1
if_then(I, T)		:- I=1, T.

% Does T, when I is true.
if_then_(I, T)		:- (I, T).
if_then_(I, _)		:- \+ I.

% Does E, when I is false.
if_else_(I, E)	:- if_then_else_(I, stuff(_), E).

% Does T, when I is equal to 1, else it does E.
if_then_else(I, T, E)	:- I=1, T, !; E.

% Does (and returns the value of) T, when I is true, else it does E.
if_then_else_(I, T, E)	:- (I, T); (\+ I, E).

% Ignores stupid stuff.
stuff(_).
% Variable has not number value (yet).
stuffn(X)	:- stuff(X), \+ number(X).
% Variable is not a list (yet).
stuffl(X)	:- stuff(X),\+ is_list(X).
% Variable is not a atom (yet).
stuffa(X)	:- stuff(X),\+ atom(X).

% NOT operator on booleans as numbers.
bool_not(1, 0).
bool_not(0, 1).

% Gets the first value of a list.
list_first(L, [L|_]).

% Appends lists A,B,C to list D.
append(A,B,C,D)	:- is_list(A),is_list(B),is_list(C), append(A,B,X), append(X,C,D).

% Writes something coloured out to the console starting with "% ".
info(X,C)	:- ansi_format([bold,fg(C)], '% ~w', [X]),writeln('').

% Uses as default colour cyan.
info(X)		:- info(X, cyan).
