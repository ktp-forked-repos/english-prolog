:- module('clauses', [en_clause/1]).
:- use_module(['verb_phrase.pl', 'noun_phrase.pl','person.pl','general.pl','object.pl','my_debug.pl','word.pl']).


% Defines clauses:
% - creates:
en_clause(X) :- stuffl(X) -> noun_phrase(N,subj,P1,P2,K),ignore_gender(P1,P2,P1_,P2_),verb_phrase_object(VP,P1_,P2_,K),append(N,VP,X).

% - checks, wheather a list X is a clause:
en_clause(X) :- is_list(X)->
		once(	(
		dbg('X',X),forall(member(M,X),word:is_word(M)),
		    en_clause_chk(X))).


en_clause_chk(X) :- append(['it'],VP,X),VP\=[], verb_phrase_object(VP,3,s,[thing]).


en_clause_chk(X) :- append(N,VP,X), N\=[],VP\=[], noun_phrase(N,subj,P1,P2,K), dbg('N:K:VP',N:K:VP),
		    ignore_gender(P1,P2,P1_,P2_), verb_phrase_object(VP,P1_,P2_,K).



































