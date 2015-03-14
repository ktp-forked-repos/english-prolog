:- module(noun_phrase, [noun_phrase/5]).
:- use_module(['adjective.pl','chars.pl','determiners.pl','demonstrative.pl','noun.pl','preposition.pl',
'pronoun.pl','quantifiers.pl','word.pl','general.pl','relative_clauses.pl','ing_phrase.pl','usage.pl','my_debug.pl']).
/*
 * Defines noun phrases:
 * X	=> noun phrase;
 * U	=> the usage: [opt.]
 * +subj=> as subject;
 * +obj => as object;
 * P1,P2=> a person [opt.];
 * K	=> the "kind" of the noun. [opt.]
*/

%noun_phrase([],_,_,_,_)	:- !, false.
% - creates:
noun_phrase(X,U,P1,P2,K)	:- stuffl(X), U	\= subj:nmod, noun_phrase_0(X,U,P1,P2,K).
%   (subject, where postmodifier: depth should be restri   cted!)
noun_phrase(X,subj:nmod,P1,P2,K):- stuffl(X), noun_phrase_0(X,subj,P1,P2,K).

% - checks (K and K2 must have joint element):
noun_phrase(X,U,P1,P2,K)	:- is_list(X),once((noun_phrase_0(X,U,P1,P2,K2),
				   (is_list(K) *-> kind_listchk(K,K2); K=K2))).

% - creates:
noun_phrase(X,subj,P1,P2,K)	:- stuffl(X), noun_phrase_0(N,subj,P1,P2,K), noun_postmodifier(P,P2,K), append(N, P, X).
% - checks:
noun_phrase(X,subj,P1,P2,K)
  :-	is_list(X), once((append(N, P, X), N\=[], P\=[], noun_phrase_0(N,subj,P1,P2,K2),
	(is_list(K) *-> kind_listchk(K,K2); K=K2), noun_postmodifier(P,P2,K2))).

% TODO: implement chaining of noun phrases...


%%% Compare with http://learnenglish.britishcouncil.org/en/english-grammar/clause-phrase-and-sentence/noun-phrase %%%

% The main part of a noun phrase:
noun_phrase_0([X],subj,3,P2,[life,thing])
				:- demonstrative(X,P2).
noun_phrase_0(X,U,P1,P2,[pers]) :- noun_phrase_p(X,U,P1,P2).
noun_phrase_0(X,_,3,P2,K)	:- noun_phrase_n(X,P2,K), must_be(list,K).

% A subject/object pronoun instead of noun phrase:
noun_phrase_p([X],subj,P1,P2)	:- subject_pronoun(P1,P2,X).
noun_phrase_p([X],obj,P1,P2)	:- object_pronoun(P1,P2,X).

% a "normal" noun phrase (with a main noun)
% - creates
noun_phrase_n(X,P2,K)		:- stuffl(X) -> noun_sp(N, P2, N2), noun_kinds(N,K), noun_phrase_(N,N2,P2,Y), append(Y,[N2],X).
% - checks:
noun_phrase_n(X,P2,K)		:- is_list(X)-> append(Y,[N2],X), noun_sp(N, P2, N2), noun_kinds(N,K), noun_phrase_(N,N2,P2,Y).

% Single noun
noun_phrase_(_,_,_,[]).

% Determiner:
noun_phrase_(_,N2,P2,[D])	:- det_an(D, P2, N2).

% Quantifier:
noun_phrase_(N,N2,P2,Q)		:- quant_noun(Q,N,N2,P2).

% Determiner + adjective:
noun_phrase_(_,_,P2,DA)		:- det_adj(DA,P2).

% Quantifier + [of] + determiner:
noun_phrase_(N,N2,P2,X)		:- quant_noun(Q,N,N2,P2), determiner(D,P2), det_an(D, P2, N2), append(Q, ['of',D], X).

% Quantifier + [of] + determiner + adjective:
noun_phrase_(N,N2,P2,X)		:- quantifier_sp(Q,P2), det_adj(A,P2), quant_noun(Q,N,N2,P2), append(Q,['of'|A],X).

/***				***/
/***	DETERMINER + A[N]	***/
/***				***/

det_an('a', s, A)	:- \+ word_vowel(A).
det_an('an',s, A)	:- word_vowel(A).
det_an(D,   P2,_)	:- determiner(D,P2).


/***				***/
/***	QUANTIFIER + NOUN	***/
/***				***/

quant_noun(Q,N,N, s)	:- quantifier_s(Q), \+ noun_is_count_able(N).
quant_noun(Q,N,N2,p)	:- noun_plural(N, N2), quantifier_cp(Q).


/***				***/
/***	DETERMINER + ADJECTIVE	***/
/***				***/

% normal
det_adj([D, A],P2)	:- is_adjective(A), det_an(D,P2,A).

% comparative:
% - short:
det_adj([D, C],P2)	:- determiner(D,P2), comparative(_,C_), C=C_, det_an(D,P2,C).
% - long:
det_adj(['the',ML,A],_)	:- \+ comparative(A, _), more_less(ML).

% superlative:
% - short:
det_adj([D, S],P2)	:- determiner(D,P2), superlative(_,S_),S=S_, det_an(D,P2,S).
% - long:
det_adj(['the',ML,A],_)	:- \+ superlative(A, _), most_least(ML).

/***					***/
/***	P O S T  M O D I F I E R	***/
/***					***/

% TODO: implement missing stuff..


/*
 * Defines postmodifiers:
 * P => post modifier [as list of words];
 * K => kind of noun.
*/

% There may be more than one postmodifier:
noun_postmodifier(P,P2,K)	:- dbg(1), noun_postmodifier_(P,P2,K).

% But in this implementation only 2 [1]:
%noun_postmodifier(P,K)	:- stuff(K), noun_postmodifier_(P1,K), noun_postmodifier_(P2,K), append(P1,P2, P).

% with a prepositional phrase:
noun_postmodifier_(PM,_,K)	:- dbg(2), prepositional_phrase(PM,n:K).

% with an –ing phrase:
noun_postmodifier_(PM,_,K)	:- dbg(3), ing_phrase(PM,K).

% with a relative clause:
noun_postmodifier_(PM,P2,K)	:- dbg(4), relative_clause(PM,P2,K,subj).

% with a that clause:


% with a to-infinitive:


% Has nothing to do with that at all:
sense_of_live(X)		:- member(X, [programming, coding, living]).
