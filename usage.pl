:- module('usage', [word_usage/2,kind_listchk/2]).
:- use_module('noun.pl').

/*
 * Gets the usage of a word:
 * W	=> the word;
 * U:WK	=> the usage : the word kind.
*/
word_usage(N, n:U)	:- noun(N), noun_get_kind(N, U).


kind_listchk(ChkList,List)	:- once(((member(M,ChkList), noun_kind_chk(List,M));
					 (member(M,List), noun_kind_chk(ChkList,M)))).
