:- module(tokens, [token/2, i18n/3]).
:- use_module(load).

% i18n(Language, Key, Translation.
% This predicate makes to translation according to the provided language
% (which can be obtained with load:option(lang/1).
i18n(english, let, let).
i18n(french, let, soit).

% token(Atom, Token).
% The token predicate converts input tokens into tokens for the parser

% Keywords
% This predicate is used to distinguish between keywords and identifiers
keyword(let).

% Ponctuation
punctuation(',').
punctuation('.').

% Actual token predicates
% Those are a green cuts because of the body of 'token(Id, ident(Id))'.

% Keywords
token(let, let) :- !.

% Punctuation
token('.', '.') :- !.
token(',', ',') :- !.

% Identifiers
token(Id, ident(Id)) :-
  % Id is an identifier only if it isn't also a keyword (otherwise it's a keyword)
  \+ keyword(Id),
  \+ punctuation(Id).
