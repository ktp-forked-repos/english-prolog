:- compile('testing.pl').
:- test_module('sentence').

test :- test_list([
'My mother is great.',
'My mother is great':fail,
'My mother is a teacher.',
'My mother is a great teacher.',
'My mother is the greatest teacher.',
'My mother is clever.',
'She is my sister.',
'That is my father.',
'That is my great father.',
'My sister is a clever':fail,
'She is my clever sister.',
'She is a good mother.',
'I is a teacher':fail,
'I am a great teacher.',
'You are a great teacher.',
'You is a great teacher.':fail,
'The better teacher is good.',
'The better is good.':fail,
'A better teacher would be good.'

],S,sentence:make_sentences(S)).
