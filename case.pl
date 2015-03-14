%%% WARNING: MODULE NOT USED!!! %%%

:- module('case', [check_case/1,is_case/1]).
/*
 * Checks, whether it is a correct case attribute.
 * Here, '0' is allowed.
*/
check_case(X) :- X=0; is_case(X).

/*
 * Checks, whether it IS a CORRECT CASE.
 * Numbers from 1 to 3 are being actepted.
*/
is_case(X)   :- X=1; X=2; X=3; X=4.
