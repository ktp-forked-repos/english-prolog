:- module(verb,[
/*
 * Defines regular verbs:
 * (1) => infinitive;
 * (2) => list of types;
*/
dynamic verb/2,

% FUNCTIONS:
verb/3,
verb_modal/1,
verb_link/2,
verb_intrans/1,
verb_trans/1,
verb_phrasal/1,
verb_phrasal/2,
verb_irregular/1,
verb_irregular/3,
verb_regular/1,
verb_prep/2,
verb_move/1,

is_verb/1,is_verb_p/1,
verb_not_be_have/1,verb_not_be/1, verb_not/2,

verb3s/2, verb_simple/4,
some_verb_past/5,
be/3, have/3,
be_participle/4, verb_participle/3,
verb_ing/2,verb_ed/2]).

:- use_module('chars.pl').
:- use_module('person.pl').

/***			***/
/***	R E T R A C T	***/
/***			***/

:- retractall(verb(_,_)).

/***				***/
/***	V E R B  T Y P E S	***/
/***				***/

verb_type(V,T)		:- verb(V,L), must_be(list,L), member(T,L).

verb_modal(V)		:- verb_type(V,modal).			% modal		(modal verb)
verb_link(V,a)		:- verb_type(V,link:a).			% followed by an adjective phrase,
verb_link(V,n)		:- verb_type(V,link:n),!.		% link verb followed by a noun phrase,
verb_link(V,a:n)	:- verb_type(V,link:a:n),!.		% or one of both.
verb_intrans(V)		:- verb_type(V,intrans).		% (intransitive verb [without object])
verb_trans(V)		:- verb_type(V,trans).			% (transitive verb [with noun phrase as object])
verb_phrasal(V)		:- verb_phrasal(V,_).			% (phrasal verb)
verb_phrasal(V,L)	:- verb_type(V,phrasal:L).		% (L is the list of noun kinds as object)
verb_prep(V,PL)		:- verb_type(V,prep:PL).		% prepositional verb (PL is list of prepositions)
verb_move(V)		:- verb_type(V,move).			% move verb (from --> to)
verb_irregular(V)	:- verb_irregular(V,_,_).		% (irregular verb)
verb_irregular(V,X,Y)	:- verb_type(V,irregular:X:Y).		% (X,Y are present and past participle)
verb_regular(V)		:- verb(V,_),\+ verb_irregular(V,_,_).	% (regular verb)

verb(A,B,C)		:- verb_type(A,irregular:B:C).

% Defines ALL verbs:
is_verb(V)		:- is_verb_p(V), !.			% present tense.
is_verb(V)		:- verb_modal(V), !.			% modal verb.
is_verb(V)		:- verb_simple(_,_,_,V).		% not infinitive.
is_verb(V)		:- verb_participle(_,V,_).		% past tense.

% Defines (normal) present tense verbs:
is_verb_p(V)		:- verb_regular(V), \+ verb_modal(V).
is_verb_p(V)		:- verb_irregular(V).

verb_not_be_have(V)	:- is_verb_p(V), V \= 'be', V \= 'have'.
verb_not_be(V)		:- is_verb_p(V), V \= 'be'.
verb_not(V, VN)		:- is_verb_p(V), V \= VN.


/***					***/
/***	V E R B  C O N J U G A T I O N	***/
/***					***/


/*
 * verb3s(I, V).
 * Defines the regular 3nd person singular forms:
 * I => infinitive verb.
 * V => conjugated verb.
*/

%%% Compare with http://www.englishtenses.com/third_person_singular %%%

% Exceptions: 1. The verb "to be"; 2. The  verb "to have"; 3. modal verbs
verb3s('be',	'is')	:- true.
verb3s('have',	'has')	:- true.
verb3s(I,	I)	:- verb_modal(I).

% The regular rest.
verb3s(I, V)	:- verb_not_be_have(I), (verb3s_1(I, V); !, verb3s_2(I,V); !, verb3s_3(I,V)).

% a) All verbs ended in -y and a vowel [not 'o'] before it, get -s.
verb3s_1(I, V)	:- vowel(Vo), Vo \='o', atom_ends_with(I, Vo), atom_concat(I, 's', V).			% Verb ends with vowel [not 'o']
verb3s_1(I, V)	:- vowel(Vo), atom_concat(Vo, 'y', E), atom_ends_with(I,E),atom_concat(I, 's', V).	% Verb ends with vowel + y.

% b) Verbs ended in -ch, -sh, -ss, -x, -o get -es.
verb3s_2(I, V)	:- (atom_ends_with(I, 'ch'); atom_ends_with(I, 'sh'); atom_ends_with(I, 'ss');
		   atom_ends_with(I, 'x'); atom_ends_with(I, 'o')), atom_concat(I, 'es', V).

% c) Verbs ended with -y and a consonant before it, get -ies.
verb3s_3(I, V)	:- consonant(C), atom_concat(C, 'y', E), atom_ends_with(I, E), atom_concat(I, 'ies', V).


/*
 * A simple verb (not a modal verb) in present:
 * (_) P1,P2	=> a person;
 * (3) V1	=> the finite verb;
 * (4) V3	=> the conjugated form of V1 in present tense.
*/
verb_simple(P1, P2,I, V)	:- I='be',	be(P1,P2, V).				% a form of 'be'
verb_simple(P1, P2,I, V)	:- I='have',	have(P1,P2, V).				% a form of 'have'
verb_simple(3,  s, I, V)	:- verb_not_be_have(I),	verb3s(I, V).			% 3rd person singular.
verb_simple(P1, P2,I, I)	:- verb_not_be_have(I),	not_p3s(P1,P2).			% not 3rd person singular.
verb_simple(P1, P2,I)		:- verb_simple(P1, P2, I,_).				% [opt.] parameter V3


/*
 * A verb in present or past participle.
 * (1)	V	=> infinitive;
 * (2)	V2	=> verb participle;
 * (_)	P1, P2	=> person;
 * (5)	Tense	=> participle (pres|past).
*/
some_verb_past(V,V2,P1, P2,Tense) :- V='be', be_participle(P1, P2, V2, Tense).
some_verb_past(V,V2, _, _, Tense) :- some_verb_past_(V,V2, Tense).
some_verb_past_(V,V2, Tense)	  :- is_verb_p(V), V\='be',verb_participle(V, V2_, Tense), V2 = V2_.

% Defines the simple forms of 'be'.
be(1, s, 'am')	:- true.
be(3, s, 'is')	:- true.
be(P1,P2,'are')	:- is_plural(P1, P2); (P1 = 2, P2 = s).


% Defines the simple forms of 'have'.
have(3, s, 'has')  :- true.
have(A, B, 'have') :- not_p3s(A, B).


/***				***/
/***	V E R B  T E N S E S	***/
/***				***/


% Defines the present and past participle forms of 'be'.
be_participle(P1, s, 'was',  pres) :- person(P1, s), P1\=2.
be_participle(P1,P2, 'were', pres) :- person(P1,P2), is_plural(P1,P2), !; P1=2.
be_participle(P1,P2, 'been', past) :- person(P1,P2).

% Gets verbs past or present participle.
verb_participle('be','been',past):- true.
verb_participle(V, Vp, pres)	 :- V\='be', verb(V, Vp, _).
verb_participle(V, Vp, past)	 :- V\='be', verb(V, _, Vp).
verb_participle(V, Vp, _)	 :- V\='be', verb_regular(V), verb_past_regular(V, Vp).

% Gets the regular present participle (eqal to past participle) of a verb.
verb_past_regular(V, Vp) :- atom_replace_ending_with(V,'e','ed',Vp),!; atom_concat(V,'ed',Vp).

/*
 * Defines the verbs -ing forms:
 * V	=> infinitive;
 * Ving	=> -ing form of V.
*/
verb_ing(V,Ving):-(vowel(Vo), consonant(C), atom_concat(Vo, C, E1), atom_concat(E1, 'e', E2), % alternating vowel and consonant + 'e'
		   atom_ends_with(V, E2), atom_replace_ending_with(V, 'e', 'ing', Ving), !); atom_concat(V, 'ing', Ving).

/*
 * Returns the regular verbs -ed form
*/
verb_ed(V, Ved)	:-	atom(V), \+ is_verb_p(V), verb_past_regular(V, Ved), !.
verb_ed(V, Ved)	:-	is_verb_p(V), verb_regular(V), verb_past_regular(V, Ved).

/***				***/
/***	A D D  V E R B S	***/
/***				***/

add_verb(A,L)	 :- atom(A), must_be(list,L), assert(verb(A,L)), !.
add_verb(A:B:C,L):- must_be(atom,A),must_be(atom,B),must_be(atom,C), must_be(list,L),append(L,[irregular:B:C],L2), assert(verb(A,L2)).

/***				***/
/***	S O M E  V E R B S	***/
/***				***/


% TODO: Add verb types below...
:- add_verb('can',			[modal]).
:- add_verb('could',			[modal]).
:- add_verb('should',			[modal]).
:- add_verb('would',			[modal]).
:- add_verb('be',			[link:a:n]).
:- add_verb('create',			[trans, intrans]).
:- add_verb('terminate',		[trans]).
:- add_verb('go':'went':'gone',		[move]).
:- add_verb('make':'made':'made',	[trans]).
:- add_verb('have':'had':'had',		[trans]).
