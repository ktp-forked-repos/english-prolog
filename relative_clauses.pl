:- module('relative_clause',[relative_pronoun/3,is_relative_pronoun/1,relative_clause/4]).
:- use_module('general.pl').
:- use_module('object.pl').

% Compare with http://www.ego4u.com/en/cram-up/grammar/relative-clauses
% Compare with http://learnenglish.britishcouncil.org/en/english-grammar/pronouns/relative-pronouns

% (1) kind (of noun);
% (2) usage (subj|obj|poss);
% (3) relative pronoun.
relative_pronoun(pers,	subj,	'who').
relative_pronoun(pers,	obj,	'who').
relative_pronoun(pers,	obj,	'whom').
relative_pronoun(pers,	poss,	'whoese').
relative_pronoun(thing,	subj,	'which').
relative_pronoun(thing,	obj,	'which').
relative_pronoun(K,	subj,	'that')	:- K=pers; K=thing.
relative_pronoun(K,	subj,	'that')	:- K=pers; K=thing.
relative_pronoun(thing,	obj,	'which').
relative_pronoun(thing,	obj,	'which').

is_relative_pronoun(RP)	:- relative_pronoun(_,_,RP).

% (1) relative clause (list of atoms);
% (2) list of noun kinds;
% (3) count (s|p);
% (4) usage (subj|obj|poss).
relative_clause([RP|VPO],P2,NKL,subj) :- member(NK,NKL), relative_pronoun(NK,subj,RP), verb_phrase_object(VPO,3,P2,NKL).
