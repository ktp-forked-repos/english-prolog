% This is the .pl file meant to be started using the command 'prolog generic.pl'.
:- module('generic', [compile/0,recompile/0]).
:- use_module('general.pl').
:- compile('sentence.pl').   % Import the module sentence.pl, but don't use it.
:- compile('numbers.pl').	% TODO: integrate 'numbers.pl' anywhere else!
:- use_module('my_debug.pl').

:- info('Type "?- make_sentences(X)." to create sentences.').
:- info('Type "?- make_sentences(\'[sentence]\')." to check,').
:- info(' whether [sentence] is a sentence.').

% recompiles all prolog files
compile		:- time(compile('*.pl')).

% retracts all and recompiles.
recompile	:- writendo(generic:compile), info('finished.'), !.

:- module('sentence').
