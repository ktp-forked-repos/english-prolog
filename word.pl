/***				***/
/***	D E F I N I T I O N S	***/
/***				***/

:- module('word', [

% Declares, that a word beginns (spoken) with a vowel (eg. 'hour').
dynamic word_vowel_/1,
word_vowel/1,
is_word/1]).

:- use_module([
'chars.pl', 'adjective.pl', 'demonstrative.pl', 'determiners.pl', 'noun.pl', 'possessives.pl',
'preposition.pl', 'pronoun.pl', 'verb.pl', 'quantifiers.pl']).

word_vowel(X)	:- word_vowel_(X), !; vowel(V), atom_concat(V, _, X).

%
% TODO:
% - implement a methode, that acepts known words of any kind.
%


is_word(W)
:- once((adjective:is_adjective_(W); demonstrative:is_demonstrative(W);determiner:is_determiner(W); noun:is_noun(W); possessive:is_possessive(W); preposition:is_preposition(W); pronoun:is_pronoun(W); verb:is_verb(W); quantifier:is_quantifier(W))).


/***			***/
/***	R E T R A C T	***/
/***			***/

:- retractall(word_vowel_(_)).
