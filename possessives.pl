:- module('possessive',[
	possessive_noun/3,	
	possessive_adjective/1,
	possessive_adjective/3,
	possessive_pronoun/1,
	possessive_pronoun/3,
	is_possessive/1]).

:- use_module(['pronoun.pl','noun.pl','chars.pl','my_debug.pl']).

/***					***/
/***	P O S S E S S I V E   W O R D S	***/
/***					***/

possessive_noun(N, s, X)	:- is_noun(N), atom_concat(N, '\'s', X),dbg('N:X',N:X).
possessive_noun(N, p, X)	:- possessive_noun_plural(P, X), noun_plural(N, P), dbg('N:P',N:P).
possessive_noun_plural(P, X)	:- once((atom_replace_ending_with(P, 's', 's\'s', X); atom_concat(P, '\'s', X))).


possessive_adjective(P)		:- pronoun(_,_,_,_,P,_).
possessive_adjective(P1,P2,P)	:- pronoun(P1,P2,_,_,P,_).
possessive_pronoun(P)		:- pronoun(_,_,_,_,_,P).
possessive_pronoun(P1,P2,P)	:- pronoun(P1,P2,_,_,_,P).

questions_possessive('whoese')	:- true.

is_possessive(P)	:- once((possessive_noun(_,_,P); possessive_adjective(P); possessive_pronoun(P); questions_possessive(P))).

















