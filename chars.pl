:- module('chars',[
vowel/1,char/1,char/2,consonant/1,atom_ends_with/2,atom_ends_with_vowel/1,atom_ends_with_consonant/1,
atom_first_chars/3,atom_first_chars/4,atom_last_chars/3,atom_last_chars/4,atom_replace_ending_with/4,atom_char_at/3]).

:- use_module('my_debug.pl').
:- use_module('general.pl').

% Defines vowels.
vowel('a', 'A').
vowel('e', 'E').
vowel('i', 'I').
vowel('o', 'O').
vowel('u', 'U').
vowel(X) :- vowel(X,_).
vowel(X) :- vowel(_,X).
is_vowel(X, B) :- (vowel(X), B is 1); (\+ vowel(X), B is 0).

% Defines the alphabet.
char('a', 'A').
char('b', 'B').
char('c', 'C').
char('d', 'D').
char('e', 'E').
char('f', 'F').
char('g', 'G').
char('h', 'H').
char('i', 'I').
char('j', 'J').
char('k', 'K').
char('l', 'L').
char('m', 'M').
char('n', 'N').
char('o', 'O').
char('p', 'P').
char('q', 'Q').
char('r', 'R').
char('s', 'S').
char('t', 'T').
char('u', 'U').
char('v', 'V').
char('w', 'W').
char('x', 'X').
char('y', 'Y').
char('z', 'Z').
char(X) :- char(X,_).
char(X) :- char(_,X).

% Defines consonants:
consonant(X) :- char(X), \+ vowel(X).
is_consonant(X, B) :- (consonant(X), B is 1); (\+ consonant(X), B is 0).

/***						***/
/***	M E T H O D E S  W I T H  S T R I N G S	***/
/***						***/

/*
 * Is true, wenn W ends with E:
 * W => an atom;
 * E => an atom.
 * Example:
 * atom_ends_with('abc', 'c') :- true.
*/

atom_ends_with(W, E)		:- atom_concat(_,E,W).

% Checks, whether W ends with a vowel:
atom_ends_with_vowel(W)		:- vowel(V), atom_ends_with(W, V).

% Checks, whether W ends with a consonant:
atom_ends_with_consonant(W)	:- consonant(C), atom_ends_with(W, C).

/*
 * Gets the first N numbers of an atom:
 * A => an atom;
 * N => length/count of chars ment to cut up;
 * S => an atom representing the first N chars of A.
 * R => the rest [opt.]
*/
atom_first_chars(A, N, S,R)	:- stuffn(N), atom_length(A, L), between(0, L, N), atom_concat(S, R, A), atom_length(S,N).
atom_first_chars(A, N, S,R)	:- number(N), atom_length(A, L), L >= N, atom_concat(S, R, A), atom_length(S, N).
atom_first_chars(A, N, S)	:- atom_first_chars(A, N, S,_).


/*
 * Gets the last N numbers of an atom:
 * A => an atom;
 * N => length/count of chars ment to cut up;
 * S => an atom representing the last N chars of A.
 * R => the rest [opt.]
*/
atom_last_chars(A, N, S,R)	:- stuffn(N),  atom_length(A, L), between(0, L, N), atom_concat(R, S, A), atom_length(S,N).
atom_last_chars(A, N, S,R)	:- number(N), atom_length(A, L), L >= N, atom_concat(R, S, A), atom_length(S, N).
atom_last_chars(A, N, S)	:- atom_last_chars(A, N, S,_).

/*
 * Replaces in atom whith a given ending this ending with a other atom:
 * S	=> the atom;
 * E	=> the ending of S;
 * R	=> the replacing of E;
 * X	=> the output (S with replaced ending).
*/
atom_replace_ending_with(S,E,R,X) :- atom(S), atom_concat(Start,E,S),atom_concat(Start,R,X), !.
atom_replace_ending_with(S,E,R,X) :- atom(X), atom_replace_ending_with(X,R,E,S), !.

atom_char_at(S, I, C)	:- sub_atom(S, I, 1, _, C).
