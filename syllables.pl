:- module('syllables', [syllables_counting/2, has_one_syllable/1]).
:- use_module('chars.pl').
:- use_module('general.pl').

/*
 * Counting syllables is not easy!
 * For this reason, the function is a little bit trivial.
 * I need syllables counting, because it is needed for the superlative and comparative of adjectives.
 * It is different for "short" and "long" adjectives.
 * But this purpose it works pretty well.
 * This is a logic implementation of my syllables counting algorithm for the German language (see below).
*/
syllables_counting(W,X)		:- atom_length(W, L), once((atom_char_at(W,0,C), syllables_first(C,B), syllables_counting_(W,B,L,1,B,X))).

syllables_first(C,B)		:- vowel(C),	B is 1.
syllables_first(C,B)		:- consonant(C),B is 0.

syllables_counting_(W,R,L,I,B,X):- I\=L, atom_char_at(W,I,C), syllables_char(C,B,B_,R,R_), J is I+1, syllables_counting_(W,R_,L,J,B_,X).
syllables_counting_(_,R,L,I,_,X):- I=L, X is R.	% Save R-value in X and return.

syllables_char(C,B,B_,R,R_)	:- vowel(C),	bool_not(B, N), if_then_else(N, (R_ is R+1, B_ is 1), (B_ is B, R_ is R)).
syllables_char(C,_,B_,R,R_)	:- consonant(C),B_ is 0, R_ is R.

% Just for fun :-)
has_one_syllable(W)		:- syllables_counting(W,1).

% Author: Maximilian Wuttke (who am I)
% Counts the syllables of a German word.
/*
int ZähleSilben(String wort) // Zählt die Silben eines Wortes
        {
            int r = 0;
            int max = wort.Length;
            bool b = IstVokal(wort[0]);
            if (b)
                r = 1;
            for (int i = 1; i < max; i++)
            {
                if (IstVokal(wort[i]))
                {
                    if (!b)
                    {
                        r++;
                        b = true;
                    }
                }
                else
                    b = false;
            }
            return r;
        }
*/
