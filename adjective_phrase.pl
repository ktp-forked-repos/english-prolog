:- module('adjective_phrase', [adjective_phrase/1,adjective_phrase/2]).
:- use_module(['adjective.pl','general.pl','noun.pl']).

adjective_phrase(X)	:- adjective_phrase(X,_).
% - creates:
adjective_phrase(X,T)	:- stuffl(X) -> create([],X,T).
% - checks:
adjective_phrase(X,T)	:- is_list(X)->(X\=[],once(check(X,T))).

check([], _)	:- true.	% return.
check([A|R],T)	:- is_adjective(A),mydebug:writendo(adjective:adjective_ntypechk(A,T)), \+ memberchk(A,R), \+ (adjective_opposite(A, Opp),memberchk(Opp, R)),check(R,T).

create(L,L,T):- \+once(has_rest(L,T)), !; (stuff(T), length(L,Length), Length > 0).			% Return.
create(L,R,T):- has_rest(L,T,A), append(L,[A],L3), create(L3,R,T).	% Add rest.


% It is very unusual to have more then three adjectives:
is_full(L)	:- length(L,Length),Length>=3.

has_rest(L,T)	:- has_rest(L,T,_).
% Don't repeat adjectives and don't make opposites:
has_rest(L,T,A)	:- \+ is_full(L),
			(is_adjective(A),\+ memberchk(A,L),adjective_ntypechk(A,T), \+ (adjective_opposite(A, Opp),memberchk(Opp, L))).


% TODO:
% - Make it more logical!
%   > use types (e.g. just adjectives for 'persons');		[DONE]
%   > don't accepts stupid things like 'god, bad, worst':	[DONE]
%	-> implement such adjective stuff, like opposites,	[DONE]
%	   general and specific adjectives, ...
% - Give it a order!
%   > General opinion before specific opinion
