:- module(person, [
	count/1, person/2, person_/2, person_g/2,gender/1,not_p3s/2,p3s/2,p3s_/2,is_plural/2,ignore_gender/4]).

/*
 * Defines counts:
 * s	=> singular;
 * p	=> plural.
*/
count(C)	:- C=s; C=p.

/*
 * Defines a person:
 * P1 => 1..3;
 * P2 => count.
*/
person(P1, P2)	:- (P1=1;P1=2;P1=3), count(P2).

% similar to person/2, but also acepts 3rd persons singular with 'gender'.
person_(P1,P2)	:- person(P1,P2).
person_(3:G,s)	:- gender(G).

% Acepts persons, but just 3rd persons singular having 'gender':
person_g(3, s)	:- !, false.
person_g(P1,P2)	:- person_(P1,P2).

% m => masculine;
% n => neuter;
% f => feminine.
gender(G)	:- G=m; G=n; G=f.

not_p3s(P1, P2)	:- person(P1, P2), \+ p3s(P1, P2).

p3s(3, s)	:- true.
p3s_(P1,P2)	:- p3s(P1,P2).
p3s_(3:G,s)	:- gender(G).

is_plural(P1,p)	:- person(P1, p).

% Tranforms persons having 'gender' in persons without 'gender':
ignore_gender(P1,P2,P1,P2)	:- person(P1,P2).
ignore_gender(3:G,s,3,s)	:- gender(G).

