% To use the debug-functions, simply add ":- use_module('my_debug.pl')." (without quotes) to the .pl-file.

:- module('mydebug', [dbg/1,dbg/2,dbgf/2,forall_dbg/2, use_dbg/0,writendo/1,writendof/1,must_eq/2]).
:- use_module('general.pl').

% "Enables" or "disables" debugging (see below).
:- dynamic use_dbg/1.
:- assert(use_dbg(false)).

use_dbg :- once(use_dbg(true)).

% A must be equal to B, else it writes a error to a new line when debug is enabled.
% When A is not equal to B, the function fails:
must_eq(A, A)	:- true.
must_eq(A, B)	:- A \= B, if_then_(use_dbg, (write('must_be('), write(A), write(', '), write(B), write(') failed.'))), fail.

% if debug is enabled, it writes L to a new line:
dbg(L)		:- if_then_(use_dbg, writeln(L)).

% Simple name-value output for debugging:
dbg(N, V)	:- if_then_(use_dbg, dbgf(N,V)).
% forces "debugging"
dbgf(N,V)       :- write(N), write(' = '), writeln(V).

% This is useful for testing:
% for all F: F => A; else it writes the F, where A had failed.
forall_dbg(F,A)	:- forall(F, if_else_(A, (info('forall failed at':F,red)))).

% Writes something and does it after that.
% When debug is enabled, it writes after execution, weather it succeeded or not.
writendo(X)	:- if_then_else_(use_dbg,				%if dbg
			(
			%then
			info(X, blue),
			if_else_((call(X), info(X:' OK', blue)),	%if X
			%else
			(info(X:' failed', red),false))),
			% end if
			call(X)).
			% end if.

% Like writendo/1, but doesn't need :- use_dbg.
writendof(X)    :- info(X, blue), if_else_((call(X), info(X:' OK', blue)),(info(X:' failed', red),fail)).
