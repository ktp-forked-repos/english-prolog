:- module('sentence', [sentence/1, make_sentences/1, sentence_make_upper/2, word_make_upper/2]).
:- use_module(['clause.pl', 'chars.pl', 'general.pl', 'my_debug.pl', 'word.pl']).

% TODO: implement complex sentences.
sentence(X)	:- sentence_(X).

% Defines simple sentences:
% - checks:
sentence_(X)	:- is_list(X), sentence_make_upper(LC, X), writendo(clauses:en_clause(LC)).
% - creates:
sentence_(X)	:- stuffl(X), en_clause(LC), once(sentence_make_upper(LC, X)).

% Writes all sentences out:
write_sentences		:- forall(make_sentences(Y), writeln(Y)).
write_sentences(D)	:- forall(make_sentences(Y), (writeln(Y), sleep(D))).

/*
 * Makes sentence atoms:
 * X	=> The list of atoms [opt.];
 * C	=> The char at the endn of the sentences [opt; default: '.'];
 * Y	=> the sentences as a atom.
*/

% - checks:
make_sentences(X, C, Y) :- is_list(X) -> atomic_list_concat(X, ' ', A), atom_concat(A, C, Y).
% - creates:
make_sentences(Y, C, X) :- atom(Y),	atom_concat(A, C, Y), atomic_list_concat(X, ' ', A).
% - [opt.] C => '.'
make_sentences(X, Y)	:- make_sentences(X, '.', Y).
% - checks:
make_sentences(Y)		:- atom(Y)  -> once((make_sentences(Y, X), sentence(X))).
% - creates:
make_sentences(Y)		:- Y=_, \+ atom(Y), (sentence(X), make_sentences(X, Y)).


% Turns the first letter of a sentence into upper case.
sentence_make_upper(X,[Y])	:- X=[Z], atom(Z), word_make_upper(Z,Y).
sentence_make_upper(X,Y)	:- X=[First|Rest], atom(First), word_make_upper(First, First2), append([First2], Rest, Y).
sentence_make_upper(X,Y)	:- Y=[First|Rest], atom(First), word_make_upper(First2, First), append([First2], Rest, X).

% Turns the first letter of a word into upper case.
word_make_upper(X,Y) :-	atom(X), atom_concat(First,Rest,X), atom_length(First,1),
			upcase_atom(First,First2), atom_concat(First2,Rest,Y).

word_make_upper(X,X) :- atom(X), atom_concat(First,_,X), char(_,First).

word_make_upper(X,Y) :- atom(Y), atom_concat(First,Rest,Y), char(First2,First), atom_concat(First2,Rest,X).

