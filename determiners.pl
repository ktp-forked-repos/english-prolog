:- module('determiner',[determiner/2,interrogative_determiner/2, is_determiner/1]).
:- use_module(['demonstrative.pl','possessives.pl']).

/***				***/
/***	D E F I N I T I O N S	***/
/***				***/

%%% Compare with http://learnenglish.britishcouncil.org/en/english-grammar/determiners-and-quantifiers %%%

/*
 * Specific determaters:
 * X	=> determiner;
 * P2	=> count (s/p).
*/
determiner(X,P2)	:- 	demonstrative(X,P2).
determiner('the',_)	:-	true.
%determiner('a',_)	:-	true.
%determiner('an',_)	:-	true.
determiner(X,_)		:- 	possessive_adjective(X).

% TODO: implement possessive nouns as determiners.


/***						***/
/***	S P E C I F I C  D E T E R M I N E R S	***/
/***						***/

% the definite article ==> can be 3rd person s/p;
% possessives;
% demonstratives: this, that, these, those;

% interrogative determiners: which and what ==> can be 3rd person s/p:
interrogative_determiner('which', spe).
interrogative_determiner('what',  gen).

is_determiner('a').
is_determiner('an').
is_determiner(D)	:- once((determiner(D,_); interrogative_determiner(D,_))).
