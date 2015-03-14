:- module('testing', [test_module/1,test_list/3]).
:- use_module(['../general.pl','../my_debug.pl']).


test_module(Module)
  :-
%	asserta(mydebug:use_dbg(true)),
	must_be(atom,Module), atom_concat('testing_',Module,M),
	assert(module(M, [test/0])), atom_concat('../',Module,M1),
	atom_concat(M1,'.pl',M2), writendof(use_module(M2)), module(Module),
	atom_concat('This is a testing file on on the module ',Module,Info0), atom_concat(Info0,'.',Info), info(Info, red),
	info('Type "?- test." to start.', red).

test_list(List, M, Action)
  :-	time((forall(member(M0, List), test_(M0,M,Action)),
	info('finished.'))).

test_(M0,M,Action)
  :-
	((M0=M:'fail', Exp is 0); (M0\=M:fail, Exp is 1, M=M0)),
	info('Now testing on':M0,yellow),
	(time(call(once(Action))) *-> R is 1; R is 0),
	(R =:= Exp *-> info('OK':M0,magenta); info('failed':M,red)),
	writeln('').
