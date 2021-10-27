:- module(i18n, [i18n/3]).

% i18n(Language, Key, Translation.
% This predicate makes to translation according to the provided language
% (which can be obtained with load:option(lang/1).

% All those '!' are green cuts because of the general predicate (see below).
% (CHANGE LOCATION OF THE TRANSLATIONS LATER)

% English
% Keywords
i18n(english, let, let) :- !.
i18n(english, of, of) :- !.
i18n(english, type, type) :- !.
i18n(english, 'is', 'is') :- !.
i18n(english, find, find) :- !.
i18n(english, such, such) :- !.
i18n(english, that, that) :- !.

% Type names
i18n(english, point, point) :- !.
i18n(english, line, line) :- !.
i18n(english, circle, circle) :- !.

% Relations
i18n(english, middle, middle) :- !.
i18n(english, on, on) :- !.

% French
% Keywords
i18n(french, let, soit) :- !.
i18n(french, of, de) :- !.
i18n(french, type, type) :- !.
i18n(french, 'is', est) :- !.
i18n(french, find, trouver) :- !.
i18n(french, such, tel) :- !.
i18n(french, that, que) :- !.

% Type names
i18n(french, point, point) :- !.
i18n(french, line, droite) :- !.
i18n(french, circle, cercle) :- !.

% Relations
i18n(french, middle, milieu) :- !.
i18n(french, on, sur) :- !.

% General translation predicate that says "if it isn't a keyword or a type
% name, don't bother translating anything" (punctuation or identifiers don't
% need to be translated obviously)
i18n(_, Atom, Atom) :-
  \+ tokens:keyword(Atom), \+ tokens:type_name(Atom), \+ relation:relation(_, Atom, _).

