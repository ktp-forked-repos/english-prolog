:- module(verb_phrase,[verb_phrase/3,verb_phrase/4,verb_phrase_check/3,verb_phrase_check/4]).
:- use_module('verb.pl').
:- use_module('tense.pl').
:- use_module('object.pl').
/*
 * Defines verb phrases.
 * X	=> is the verb phrase;
 * V	=> the main verb [opt.];
 * P1,P2=> a person;
*/
verb_phrase_(X,V,P1,P2)	:- tense(T1,T2,T3), is_verb_p(V), verb_phrase(T1,T2,T3,V,P1,P2,X).
verb_phrase(X,V,P1, P2)	:- verb_phrase_(X,V,P1, P2).
verb_phrase(X,	P1, P2)	:- verb_phrase(X,_,P1,P2).

% - checks verb phrases quickly:
verb_phrase_check(X,V,P1,P2)	:- check_are_verbs(X) -> once(verb_phrase(X,V,P1,P2)).
verb_phrase_check(X,P1,P2)	:- verb_phrase_check(X,_,P1,P2).

check_are_verbs(X)		:- must_be(list,X),X\=[],forall(member(V, X), is_verb(V)).
/*
 * verb_phrase(A,B,C,V,P1,P2,X)
 * 1,2,3 => tense;
 * V	 => verb;
 * P1,P2 => person;
 * X	 => verb phrase.
*/

%%% Compare with http://learnenglish.britishcouncil.org/en/english-grammar/verbs/verb-phrases %%%

% 1) a main verb (simple aspect)
verb_phrase(simp, pres, norm, V,  P1,P2,[X])	:- verb_simple(P1, P2,V, X).			% simple verb in present tense
verb_phrase(simp, past, norm, V,  P1,P2,[X])	:- some_verb_past(V, X, P1, P2, pres).		% simple verb in past tense

% 2) an auxiliary verb ("be") and a main verb in –ing form (present continuous aspect)
verb_phrase(prog, pres, norm, V, P1, P2, X)	:- X=[Be, Ving], be(P1, P2, Be), verb_ing(V, Ving).
verb_phrase(prog, past, norm, V, P1, P2, X)	:- X=[Be, Ving], be_participle(P1,P2,Be,pres), verb_ing(V, Ving), append([Be],[Ving], X).

% Passive: main verb in past participle:
verb_phrase(pass, pres, norm, V, P1, P2, X)	:- X=[Be,Vp], be(P1, P2, Be), verb_participle(V, Vp, past).
verb_phrase(pass, past, norm, V, P1, P2, X)	:- X=[Be,Vp], be_participle(P1,P2,Be,pres), verb_participle(V, Vp, past).

% 3) an auxiliary verb ("have") and a main verb with past participle (perfect aspect)
verb_phrase(simp, pres, perf, V, P1, P2, X)	:- X=[Have, Vp], have(P1, P2, Have), some_verb_past(V, Vp, P1, P2, past).
verb_phrase(simp, past, perf, V, P1, P2, X)	:- X=['had',Vp], some_verb_past(V, Vp, P1, P2, past).

% 4) an auxiliary verb ("have" + "been") and a main verb in the –ing form (perfect continuous aspect)
verb_phrase(prog, pres, perf, V, P1, P2, X)	:- X=[Have,'been',Ving], have(P1, P2, Have), verb_ing(V, Ving).
verb_phrase(prog, past, perf, V, _, _, X)	:- X=['had','been',Ving],verb_ing(V, Ving).

% perfect:
verb_phrase(pass, pres, perf, V, P1, P2, X)	:- X=[Have,'been',Vp], have(P1, P2, Have), verb_participle(V, Vp, past).
verb_phrase(pass, past, perf, V, _, _, X)	:- X=['had','been',Vp], verb_participle(V, Vp, past).

% 5) a modal verb [...] and a main verb:
verb_phrase(simp, cond, norm, V, _, _, X)	:- X=[M, V], verb_modal(M).

% 6) We can use modal verbs with the auxiliaries "be", "have", and "have been":
verb_phrase(simp, cond, perf, V, _, _, X)	:- X=[M,'have',Ved], verb_modal(M), verb_participle(V, Ved, past).
verb_phrase(simp, cond, perf, V, _, _, X)	:- X=[M,'be',Ving], verb_modal(M), verb_ing(V, Ving).
verb_phrase(simp, cond, perf, V, _, _, X)	:- X=[M,'have','been',Ving], verb_modal(M), verb_ing(V, Ving).

% perfect:
verb_phrase(pass, cond, perf, V, _, _, X)	:- X=[M,'be',Vp], verb_modal(M), verb_participle(V, Vp, past).
verb_phrase(pass, cond, perf, V, _, _, X)	:- X=[M,'have','been',Vp], verb_modal(M), verb_participle(V, Vp, past).

% TODO:
% - implement passive!		[DONE]
% - implement object adding!	[DONE]
% - implement negation!
