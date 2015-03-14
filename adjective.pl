:- module('adjective', [

% Defines adjectives:
% (1)	is atom		=> A is regular;
%  :	has form P:C:S	=> A is unregular;
% (2)	is adjective stuff [see below].
dynamic adjective/2,

is_adjective/1,is_adjective/2,
is_adjective_/1,
adjective_stuff/2,adjective_ntype/2,adjective_ntypes/2, adjective_opposite/2,
adjective_regular/1,adjective_irregular/3,
adjective_ntypechk/2,
more_less/1, most_least/1,
comparative/2, superlative/2]).

:- use_module(['verb.pl','chars.pl','syllables.pl','usage.pl','my_debug.pl']).

/***				***/
/***	D E F I N I T I O N S	***/
/***				***/


/*
 * Is A a adjective?
 * (1) A  => adjective;
 * (2) S  => stuff [opt].
*/
is_adjective(A)		:- is_adjective(A,_).
is_adjective(A,S)	:- adjective(A,S), atom(A).

% A can also be a comparative or superlative form:
is_adjective_(A)	:- is_adjective(A).
is_adjective_(A)	:- comparative(_, X), X=A.
is_adjective_(A)	:- superlative(_, X), X=A.



/***				***/
/***	N A M E D  S T U F F	***/
/***				***/

% op:gen	=> general opinion
% op:spec	=> specific opinion
% use:afterlink	=> only after a link verb.
% use:infr_noun	=> only in front of a noun.	[not used?]
% length:short	=> a short adjective [which my algorithm cannot recognize].
% length:long	=> a long adjective.
% ntype:[...]	=> the adjective stands for specific noun types.
% opp:OPP	=> A is opposite of B <==> B is opposite of A.
% rise:R	=> R is stronger than A, e.g. R:A => good:great
% irregular:C:S => A is a irregular adjective, C and S are superlative and comparatie

% Has the adjective in its stuff-list a "specific stuff" value?
adjective_stuff(A,SpSt)	:- is_adjective(A, StffLst), member(SpSt,StffLst).

adjective_ntypes(A,NTL)	:- is_adjective(A),\+adjective_stuff(A,use:afterlink), adjective_stuff(A,ntype:NTL).

% [see noun:noun_kind_chk/2]
adjective_ntypechk(A,TL):- is_list(TL), adjective_ntypes(A,NTL), kind_listchk(NTL,TL).

adjective_ntype(A,NT)	:- is_adjective(A),\+adjective_stuff(A,use:afterlink), adjective_stuff(A,ntype:NTL), is_list(NTL),
			   atom(NT), member(NT,NTL).

adjective_ntype(A,NTL1)	:- is_adjective(A),\+adjective_stuff(A,use:afterlink), adjective_stuff(A,ntype:NTL), is_list(NTL),
			   is_list(NTL1), kind_listchk(NTL1,NTL).

adjective_opposite(A,B)	:- once(adjective_opposite_d(A,B); adjective_opposite_d(B,A)).
adjective_opposite(A,B)	:- once(adjective_opposite_i(A,B); adjective_opposite_i(B,A)).

% Direct opposite:
adjective_opposite_d(A,B):- is_adjective(A),is_adjective(B), adjective_stuff(A,opp:B).
% Indirect opposite:
adjective_opposite_i(A,B):- is_adjective(A),is_adjective(B), is_adjective(R), adjective_stuff(A,rise:R), adjective_opposite(R,B).

% Defines, that it is a short/long adjective.
adjective_short(A)	:- adjective_stuff(A,length:short), !.
adjective_short(A)	:- is_adjective(A), syllables_counting(A,X), X<3.
adjective_long(A)	:- adjective_stuff(A,length:long), !.
adjective_long(A)	:- is_adjective(A), syllables_counting(A,X), X>2.

adjective_regular(A)		:- \+ adjective_stuff(A,irregular:_).
adjective_irregular(A,B,C)	:- adjective_stuff(A,irregular:B:C).

more_less(ML)	:- ML='more'; ML='less'.
most_least(ML)	:- ML='most'; ML='least'.


/***			***/
/***	R E T R A C T	***/
/***			***/

:- retractall(adjective(_,_)).

/***				***/
/***	C O M P A R A T I V E	***/
/***				***/

/*
 * Defines the regular comperative forms of adjectives.
 * (1)	A	=> positive;
 * (2)	C	=> comperative.
*/

%%% Compare with http://www.insegnanti-inglese.com/grammar-1/comp-super.html %%%

% Some very common adjectives have irregular comparative forms:

comparative(A, C)	:- adjective_irregular(A,C,_).

% regular
comparative(A, C)	:- is_adjective(A),adjective_regular(A), adjective_short(A), comparative_short(A,C).

comparative_short(A,C)	:-	comparative_short1(A,C), !;
				comparative_short2(A,C), !;
				comparative_short3(A,C), !;
				comparative_short4(A,C).

% If the adjective ends with a single vowel +consonant, we double the consonant and add "--er":
comparative_short1(A,C)	:- consonant(C0), vowel(C1), C0 \= C1, consonant(C2), atom_concat(C0, C1, E0), atom_concat(E0, C2, E),
			   atom_concat(_,E,A), atom_concat(A, C2, C3), atom_concat(C3, 'er',C).

% If the base adjective ends in "--y" we replace the "y" with "ier":
comparative_short2(A,C)	:- atom_replace_ending_with(A,'y','ier',C).

% If the base adjective ends in "--e" we only add an "r":
comparative_short3(A,C)	:- atom_concat(_,'e',A), atom_concat(A,'r',C).

% Most one-syllable adjectives and some two-syllable adjectives take the ending '--er'.
comparative_short4(A,C)	:- atom_concat(A, 'er', C), \+ (atom_ends_with(A, 'y'); atom_ends_with(A, 'e')).


/***				***/
/***	S U P E R L A T I V E	***/
/***				***/

% Some very common superlatives have irregular forms:
superlative(A, S)	:- adjective_irregular(A,_,S).

% regular
superlative(A, S)	:- is_adjective(A),adjective_regular(A), adjective_short(A), superlative_short(A,S).

superlative_short(A,C)	:- 	superlative_short1(A,C), !;
				superlative_short2(A,C), !;
				superlative_short3(A,C), !;
				superlative_short4(A,C).

% If the adjective ends in a single vowel + consonant, we double the consonant and add"--est":
superlative_short1(A,C)	:- consonant(C0), vowel(C1), consonant(C2), atom_concat(C0, C1, E0), atom_concat(E0, C2, E),
			   atom_concat(_,E,A), atom_concat(A, C2, C3), atom_concat(C3, 'est',C).

% If the adjective ends in '--y' we replace the 'y' with 'iest' :
superlative_short2(A,C)	:- atom_replace_ending_with(A,'y','iest',C).

% If the adjective already ends in '--e' we only add '--st' :
superlative_short3(A,C)	:- atom_concat(_,'e',A), atom_concat(A,'st',C).

% One-syllable adjectives (and some common two-syllable adjectives) become superlative by adding the ending '--est'.
superlative_short4(A,C)	:- atom_concat(A, 'est', C), \+ (atom_concat(_,'y',A); atom_concat(_,'e',A)).

comparative_superlative(A:C:S) :- is_adjective(A), comparative(A, C), superlative(A, S).

/***			***/
/***	A D D I N G S	***/
/***			***/

% Adds an adjective:
add_adjective(A,T)	:- atom(A), assert(adjective(A,T)).
add_adjective(A:B:C,T)	:- atom(A),atom(B),atom(C), append(T,[irregular:B:C],T2), assert(adjective(A,T2)).

/***					***/
/***	S O M E  A D J E C T I V E S	***/
/***					***/

:- add_adjective('big',				[ntype:[thing, life]]).
:- add_adjective('great',			[ntype:[thing, life],rise:'big',rise:'good']).
:- add_adjective('hot',				[ntype:[thing]]).
:- add_adjective('good':'better':'best',	[ntype:[thing,pers], opp:'bad']).
:- add_adjective('bad':'worse':'worst',		[ntype:[thing,pers], opp:'good']).
:- add_adjective('interesting',			[ntype:[thing]]).
:- add_adjective('nice',			[ntype:[life]]).
:- add_adjective('beautiful',			[ntype:[life]]).
:- add_adjective('clever':cleverer:cleverest,	[ntype:[pers]]).

