:- module('pronoun', [pronoun/6, subject_pronoun/3,object_pronoun/3, is_pronoun/1]).

/*
 * Defines personal pronouns:
 * (1,2)person;
 * (3)	subject;
 * (4)	object;
 * (5)	possessive adjectives;
 * (6)	possessive pronouns.
*/

pronoun(1,s,	'I',	'me',	'my',	'mine').
pronoun(2,s,	'you',	'you',	'your',	'yours').
pronoun(3:m,s,	'he',	'him',	'his',	'his').
pronoun(3:f,s,	'she',	'her',	'her',	'hers').
pronoun(3:n,s,	'it',	'it',	'its',	'its').
pronoun(1,p,	'we',	'us',	'our',	'ours').
pronoun(2,p,	'you',	'you',	'your',	'yours').
pronoun(3,p,	'they',	'them',	'their','theirs').

subject_pronoun(P1,P2, P)	:- pronoun(P1,P2,P,_,_,_).
object_pronoun(P1,P2, P)	:- pronoun(P1,P2,_,P,_,_).


is_pronoun(P) :- once((pronoun(_,_,A,B,C,D), memberchk(P,[A,B,C,D]))).

% TODO:
% - indefinite pronouns	: http://learnenglish.britishcouncil.org/en/english-grammar/pronouns/indefinite-pronouns
% - reciprocal pronouns	: http://learnenglish.britishcouncil.org/en/node/1293
%
%
% For relative pronouns see [relative_clauses.pl]

