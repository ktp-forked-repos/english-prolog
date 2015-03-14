:- module(numbers, [quant_number/2]).

:- use_module(['general.pl', 'chars.pl', 'my_debug.pl']).

:- retractall(numbers(_,_,_)).

numbers(X,Y)	:- numbers(X,Y,_).

numbers(0,	'zero', x).
numbers(1,	'one','first').
numbers(2,	'two', 'second').
numbers(3,	'three', 'third').
numbers(4,	'four', x).
numbers(5,	'five', x).
numbers(6,	'six', x).
numbers(7,	'seven', x).
numbers(8,	'eight', x).
numbers(9,	'nine', x).
numbers(10,	'ten', x).
numbers(11,	'eleven', x).
numbers(12,	'twelve', x).
numbers(13,	'thirteen', x).
numbers(14,	'fourteen', x).
numbers(15,	'fifteen', x).
numbers(16,	'sixteen', x).
numbers(17,	'seventeen', x).
numbers(18,	'eighteen', x).
numbers(19,	'nineteen', x).
numbers(20,	'twenty', x).
numbers(30,	'thirty', x).
numbers(40,	'forty', x).
numbers(50,	'fifty', x).
numbers(60,	'sixty', x).
numbers(70,	'seventy', x).
numbers(80,	'eighty', x).
numbers(90,	'ninety', x).
numbers(100,	'hundret', x).
numbers(e:2,	'thousand', x).
numbers(e:3,	'million', x).



% N is Number
% X is Atom
% N->X
quant_number(N, X)	:-	number(N), atom_number(A, N), quant_number_(A, X).
% X->N
quant_number(N, X)      :-	N=_, \+ number(N), is_list(X),forall(member(M, X), atom(M)), once((between(0,999999999,N), quant_number(N, X))).

quant_number_(A,X)	:-	atom(A), atom_length(A, L), C is ceiling(L/3), M is L mod 3,
				if_then_(M=2, atom_concat('0',A,A2)),
				if_then_(M=1, atom_concat('00',A,A2)),
				if_then_(M=0, A2=A),
				numbers_groups(A2, C, X).

numbers_groups('',0,[])	:- true.	% RETURN.

numbers_groups(A, C, Y)	:-
	atom_first_chars(A, 3, First, Rest), atom_number(First, H), numbers_999(First, Y1),
	C2 is C - 1, numbers_groups(Rest, C2, Y3),
	if_then_else_((C>1, H>0),
	(
	numbers:numbers(e:C, Y2_),

	if_then_else_(H>1,
		atom_concat(Y2_,'s',Y2),
		Y2 = Y2_),
	append(Y1, [Y2], Y3, Y)
	), append(Y1, Y3, Y)).




numbers_999(X,Y)	:-	atom_length(X, L), numbers_999(X,Y,L).

numbers_999(X,Y,3)	:-	atom_concat('0', Rest, X), numbers_99(Rest, Y, 2), !.

numbers_999(X,Y,3)	:-	atom_chars(X, [Hundrets|_]), atom_concat(Hundrets, Rest, X), atom_number(Hundrets, H),
				numbers(H, Y1), if_then_else_(H>1, Y2='hundrets', Y2='hundret'),
				numbers_99(Rest, Y3), append([Y1], [Y2|Y3], Y).

numbers_999(X,Y,L)	:-	between(1, 2, L), numbers_99(X, Y, L).

numbers_99(X,Y)		:-	atom_length(X,L), numbers_99(X,Y,L).

numbers_99(X,Y,2)	:-	atom_concat('0', Rest, X), numbers_9(Rest, Y), !.

numbers_99(X,[Y],2)	:-	atom_number(X,N), between(1,19,N), numbers(N, Y).

numbers_99(X, Y, 2)	:-	atom_number(X,N), N>19, atom_concat(First, Last, X),
				atom_number(First, F), F0 is F * 10, numbers(F0, Y1), numbers_99(Last,Y2), append([Y1],Y2,Y).

numbers_99(X,Y, 1)	:-	numbers_9(X,Y).

numbers_9('0',[])	:-	true, !.
numbers_9(X,[Y])	:-	atom_length(X, 1), atom_number(X, N), between(1,9,N), numbers(N, Y).


%number_file(F)	:-	open(F, write, S),
%			forall(
%				(between(0, 1000000, N), quant_number(N,Y)),
%				(atomic_list_concat(Y, ' ', Z),write(S,Z), nl(S))
%				),
%			close(S).


% Just for testing purposes...
%test_numbers	:-	forall_dbg(between(0,999999,N),(quant_number(N, _))).


% TODO:
% - implement such things as first, second, third, ...
