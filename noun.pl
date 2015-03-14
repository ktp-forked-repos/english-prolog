:- module('noun',[
% Defines nouns:
% (1) noun;
% (2) "noun stuff".
dynamic noun/2,
is_noun/1,
noun_sp/3,
count_able/1,
noun_is_count_able/1,
noun_kinds/2,
noun_kind/2,
noun_kind_chk/2,
noun_plural/2]).

:- use_module(['chars.pl', 'word.pl']).

is_noun(N)	:- noun(N,_).

/*
 * Gets nouns singular or plural form:
 * (1) N => a noun;
 * (2) C => a count;
 * (3) X => singular or plural of N.
*/
noun_sp(N,s,N)	:- noun(N,_).
noun_sp(N,p,P)	:- noun(N,_), noun_plural(N, P).


/***				***/
/***	N A M E D  S T U F F	***/
/***				***/

% cn		=> count noun.
% use:cn	=> use it as count count noun.
% use:s		=> it can be used as singular only.
% p:P		=> P is plural.
% g:G		=> G is the gender.
% kind:[K]	=> K is the list of kinds:
%	> anim,	(animal)
%	> name,	(name)
%	> pers,	(person)
%	> thing,(thing)
%	> place,(place)
%	> time,	(time)

% Has the noun in its stuff-list a "specific stuff" value?
noun_stuff(N,SpSt)	:- noun(N, StffLst), member(SpSt,StffLst).

% Checks, whether N is a count noun or whether N is used as a count noun.
count_able(N)		:- noun_stuff(N, cn); noun_stuff(N, use:cn).

% Checks, whether N is a real count noun.
noun_is_count_able(N)	:- noun_stuff(N, cn).

% Get the list of kinds of a noun.
% If there is no kind declared, the default value is ['thing'].
noun_kinds(N,NKL)	:- noun_stuff(N, kind:NKL), must_be(list, NKL).
noun_kinds(N,[thing])	:- \+ noun_stuff(N, kind:_).

% Gets the kind of a noun.
% If the kind is 'anim', 'pers' or 'name', it adds the NK-value 'life'.
noun_kind(N,NK)		:- noun_kinds(N,NKL), noun_kind_chk(NKL,NK).

% Checks, whether NK is kind which is also in the list NKL.
noun_kind_chk(NKL,NK)	:- member(NK,NKL).
noun_kind_chk(NKL,life)	:- (K=anim; K=pers; K=name), memberchk(K, NKL).



/***			***/
/***	R E T R A C T	***/
/***			***/

:- retractall(noun(_,_)).


/***				***/
/***	P L U R A L  F O R M S	***/
/***				***/

noun_plural(N,P)	:- noun_is_count_able(N), \+ noun_stuff(N,use:s), noun_plural_(N,P).

% unregular
noun_plural_(N,P)	:- noun_stuff(N,p:P_), P = P_.
%regular
noun_plural_(N,P)	:- \+noun_stuff(N,p:_), noun_plural_R(N,P).

noun_plural_R(N,P)	:- noun_plural_1(N, P), !; noun_plural_2(N, P), !; noun_plural_3(N, P), !; noun_plural_0(N, P).

%%% Compare with http://learnenglish.britishcouncil.org/en/english-grammar/nouns/count-nouns %%%

% We usually add –s to make a plural noun:
noun_plural_0(N, P)	:- atom_concat(N, 's', P).

% We add -es to nouns ending in –ss; -ch; -s; -sh; -x
noun_plural_1(N, P)	:- (E='ss'; E='ch'; E='s'; E='sh'; E='x'), atom_ends_with(N, E), atom_concat(N, 'es', P).

% When a noun ends in a consonant and -y we make the plural in -ies...
noun_plural_2(N, P)	:- consonant(C), atom_concat(C, 'y', E), atom_ends_with(N, E), atom_replace_ending_with(N,'y','ies',P).

% but if a noun ends in a vowel and -y we simply add -s:
noun_plural_3(N, P)	:- vowel(V), atom_concat(V, 'y', E), atom_concat(_,E,N), atom_concat(N, 's', P).


/***			***/
/***	A D D I N G	***/
/***			***/

add_noun(N)	:- must_be(atom,N), assert(noun(N)).
add_noun(N, L)	:- must_be(atom,N), must_be(list,L), assert(noun(N,L)).

/***			***/
/***	N O U N S	***/
/***			***/

:- add_noun('mother',	[kind:[pers],	cn,	g:f]).
:- add_noun('father',	[kind:[pers],	cn,	g:m]).
:- add_noun('sister',	[kind:[pers],	cn,	g:f]).
:- add_noun('teacher',	[kind:[pers],	cn]).
:- add_noun('apple',	[kind:[thing],	cn]).
:- add_noun('coffee',	[kind:[thing],	use:cn]).
:- add_noun('ape',	[kind:[anim],	cn]).
:- add_noun('cat',	[kind:[anim],	cn]).
:- add_noun('dog',	[kind:[anim],	cn]).
:- add_noun('man',	[kind:[pers],	cn,	g:m,	p:'men']).
:- add_noun('woman',	[kind:[pers],	cn,	g:f,	p:'women']).
:- add_noun('condition',[kind:[thing],	cn]).
