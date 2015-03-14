:- module('preposition', [
% Defines prepositions:
% 1	=> preposition;
% 2	=> list of usages ==> see [usage.pl].
dynamic preposition/2,
is_preposition/1,
prepositional_phrase/2,
prepositional_phrase/3]).

:- use_module('general.pl').

is_preposition(P)	:- once(preposition(P,_)).


/***							***/
/***	P R E P O S I T I O N A L  P H R A S E S	***/
/***							***/

%%% Compare with http://learnenglish.britishcouncil.org/en/english-grammar/clause-phrase-and-sentence/prepositional-phrases %%%

/*
 * A prepositional phrase is made up of a preposition and a noun phrase [used as an object]:
 * P	=> prepositional phrase;
 * X	=> the preposition [opt.];
 * U	=> usage (subj|obj).
*/
prepositional_phrase(P,U)	:- prepositional_phrase(P,_,U).
prepositional_phrase(P,X,U)	:- U=n:Z,	atom(Z),stuff(Z), preposition(X, L),member(U, L),noun_phrase(Y,obj),	append([X], Y, P).
prepositional_phrase(P,X,U)	:- U=n:Z:NK2,	atom(Z),stuff(Z), preposition(X, L),member(U, L),noun_phrase(Y,obj:NK2),append([X], Y, P).


/***			***/
/***	R E T R A C T	***/
/***			***/

:- retractall(preposition(_,_)).


/***			***/
/***	A D D I N G S	***/
/***			***/

add_preposition(X,U)	:- assert(preposition(X,U)).

:- add_preposition('in',	[n:thing]).
:- add_preposition('on',	[n:thing]).
:- add_preposition('from',	[n:life:place]).
:- add_preposition('to',	[n:place]).
