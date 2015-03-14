:- module('demonstrative', [demonstrative/2,is_demonstrative/1]).

%%% Compare with http://learnenglish.britishcouncil.org/en/english-grammar/pronouns/that-these-and-those %%%

% demonstratives: this, that, these, those 

% We use this (singular) and these (plural) as pronouns:
% - to talk about people or things near us;
% - to introduce people;
% - to introduce ourselves to begin a conversation on the phone;
demonstrative('this',	s).
demonstrative('these',	p).

% We use that (singular) and those (plural):
% - to talk about things that are not near us;
% - We also use that to refer back to something someone said or did;
demonstrative('that',	s).
demonstrative('those',	p).

is_demonstrative(D) :- demonstrative(D,_).
