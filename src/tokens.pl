:- module(tokens, [token/2, i18n/3]).
:- use_module(load).

% i18n(Language, Key, Translation.
% This predicate makes to translation according to the provided language
% (which can be obtained with load:option(lang/1).

% All those '!' are green cuts because of the general predicate (see below).
i18n(english, let, let) :- !.
i18n(french, let, soit) :- !.

i18n(english, point, point) :- !.
i18n(french, point, point) :- !.
i18n(english, line, line) :- !.
i18n(french, line, droite) :- !.
i18n(english, circle, circle) :- !.
i18n(french, circle, cercle) :- !.

% General translation predicate that says "if it isn't a keyword or a type
% name, don't bother translating anything" (punctuation or identifiers don't
% need to be translated obviously)
i18n(_, Atom, Atom) :- \+ keyword(Atom), \+ type_name(Atom).

% token(Atom, Token).
% The token predicate converts input tokens into tokens for the parser

% Keywords
% This predicate is used to distinguish between keywords and identifiers
keyword(let).

% Ponctuation
punctuation(',').
punctuation('.').

% Type names
type_name(point).
type_name(line).
type_name(circle).


% Actual token predicates
% Those are a green cuts because of the body of 'token(Id, ident(Id))'.

% Keywords
token(Kw, keyword(Kw)) :- keyword(Kw).

% Punctuation
token(P, punctuation(P)) :- punctuation(P).

% Type names
token(X, type_name(X)) :- type_name(X).

% Identifiers
token(Id, ident(Id)) :-
  % Id is an identifier only if it isn't also a keyword (otherwise it's a keyword)
  \+ keyword(Id),
  \+ punctuation(Id).
