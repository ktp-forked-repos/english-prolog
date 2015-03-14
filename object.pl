:- module('object',
[verb_object/3, verb_phrase_object/4]).
:- use_module('verb.pl').
:- use_module('noun.pl').
:- use_module('noun_phrase.pl').
:- use_module('adjective.pl').
:- use_module('adjective_phrase.pl').
:- use_module('my_debug.pl').
:- use_module('general.pl').
:- use_module('verb_phrase.pl').
:- use_module('preposition.pl').

% - create:
verb_object(V,O,NT)	:- stuffl(O), verb_pattern(V,L), dbg('L',L), pattern(V,NT,L,O), O\=[].
% - check:
verb_object(V,O,NT)	:- is_list(O), verb_pattern(V,L), dbg('L:NT',L:NT), once(pattern(V,NT,L,O)).


pattern(_, _, [], [])	:- true, !.
pattern(V,NT, [P], Y)	:- dbg('[P]',[P]), pattern(V,NT,P,Y).
pattern(_, NT,[P|R],Y)	:- stuffl(Y) -> (dbg('[P|R]',[P|R]), pattern(V,NT,P,Y1),pattern(V,NT,R,Y2), append(Y1, Y2, Y)).
pattern(_, NT,[P|R],Y)	:- is_list(Y)-> (R\=[], dbg('[P|R]',[P|R]), append(Y1, Y2, Y),Y1\=[],Y2\=[],pattern(V,NT,P,Y1),pattern(V,NT,R,Y2)).
pattern(_,NT, n:subj,Y) :- dbg('NT:U:Y',NT:subj:Y),noun_phrase:noun_phrase(Y,subj:nmod, 3,_,NT).
pattern(_,NT, n:obj, Y) :- dbg('NT:U:Y',NT:obj:Y), noun_phrase:noun_phrase(Y,obj, 3,_,NT).
pattern(_,NT, a, Y)	:- dbg('NT:Y',NT:Y),adjective_phrase:adjective_phrase(Y,NT).
pattern(_,_,p:P,[P])	:- is_preposition(P).

verb_pattern(V,[])		:- verb_intrans(V).
verb_pattern(V,[n:obj])		:- verb_trans(V).
verb_pattern(V,[a])		:- (A=a;A=a:n), verb_link(V, A).
verb_pattern(V,[n:subj])	:- (A=n;A=a:n), verb_link(V, A). % No postmodifier
verb_pattern(V,[p:P,n:obj])	:- verb_phrasal(V,PL),	member(P,PL).
verb_pattern(V,[n:obj,p:P])	:- verb_phrasal(V,PL),	member(P,PL).
verb_pattern(V,[p:P])		:- verb_prep(V,PL),	member(P,PL).
verb_pattern(V,P)		:- (P=[p:to,n]; P=[p:from,n,p:to,n]), verb_move(V).

/*
 * Makes a verb phrase with or without an object.
 * VP	=> verb phrase [+ object];
 * P1,P2=> person;
 * NT	=> noun type/kind.
*/
% verb phrase:
% - without object:
verb_phrase_object(VP,P1_,P2_,_)  :- is_list(VP),once(verb_phrase_check(VP,P1_,P2_)).
verb_phrase_object(VP,P1_,P2_,_)  :- stuffl(VP), verb_phrase(VP,P1_,P2_).
% - with object:
%   > checks:
verb_phrase_object(VP,P1_,P2_,NT) :- is_list(VP),append(V, O, VP), V\=[],O\=[], dbg('V:O',V:O),
				     verb_phrase_check(V,Verb,P1_,P2_),dbg('Verb',Verb),verb_object(Verb,O,NT).
%   > creates:
verb_phrase_object(VPO,P1_,P2_,NT):- stuffl(VPO),verb_phrase(VP,V,P1_,P2_), verb_object(V,O,NT), append(VP, O, VPO).

