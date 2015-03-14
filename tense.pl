:- module('tense', [tense/3,tense/2,is_participle/1]).
/*
 * Defines English tenses:
 *
 * X => "simp" (simple); "prog" (progressive); "pass" (passive); 
 * Y => "pres" (present);"past" (past); "will" (future) or "cond" (conditional);
 * Z => "norm" (normal); "perf" (perfect) [opt].
*/
tense(X,Y,Z) :- (X=simp; X=prog; X=pass), (Y=pres; Y=past; Y=will; Y=cond), (Z=norm; Z=perf).
tense(X,Y)   :- tense(X,Y, norm).	% [opt.] Z => norm.
is_participle(Y) :- Y=past; Y=pres.

