:- module('ing_phrase', [ing_phrase/2]).

:- use_module(['verb.pl', 'verb_phrase.pl', 'object.pl', 'my_debug.pl', 'general.pl']).


ing_phrase([Ving|Object],NK)
  :-	(is_list(Object) *-> Object\=[]; stuffl(Object)),
	is_verb_p(Verb), dbg('Verb',Verb), verb_ing(Verb, Ving_),Ving=Ving_, verb_object(Verb,Object,NK).
