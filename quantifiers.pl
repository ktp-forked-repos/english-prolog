:- module('quantifier', [
/*
 * quantifier for count nouns:
 * (1)	[quantifier];
 * (2)	s|p|sp.
*/
dynamic quantifier_c/2,
/*
 * quantifier for uncount nouns:
 * (1)	[quantifier].
*/
dynamic quantifier_uc/1,
quantifier/1,
quantifier_cp/1,
quantifier_s/1,
quantifier_p/1,
quantifier_sp/2,
is_quantifier/1]).

% Defines quantifiers:
quantifier(Q)	:- quantifier_c(Q,_); quantifier_uc(Q).
is_quantifier(Q):- quantifier(Q).

% Defines quantifiers for plural nouns:
quantifier_cp(Q)	:- quantifier_c(Q, p).
quantifier_cp(Q)	:- quantifier_c(Q, sp).



quantifier_s(Q)		:- quantifier_c(Q, s); quantifier_c(Q, sp); quantifier_uc(Q).
quantifier_p(Q)		:- quantifier_c(Q, p); quantifier_c(Q, sp).
quantifier_sp(Q,s)	:- quantifier_s(Q).
quantifier_sp(Q,p)	:- quantifier_p(Q).


/***			***/
/***	R E T R A C T	***/
/***			***/

:- retractall(quantifier_c(_,_)).
:- retractall(quantifier_uc(_)).

/***					***/
/***	A D D  Q U A N T I F I E R	***/
/***					***/

add_quantifier(Q, R)		:- atom(Q), add_quantifier([Q], R).
add_quantifier([Q], uc)		:- assert(quantifier_uc([Q])).
add_quantifier([Q], c:C)	:- assert(quantifier_c([Q], C)).
add_quantifier([Q], c:C:uc)	:- add_quantifier([Q], uc), add_quantifier([Q], c:C).

/***					***/
/***	S O M E  Q U A N T I F I E R	***/
/***					***/
:- add_quantifier('much',	uc).
:- add_quantifier('many',	c:p).
