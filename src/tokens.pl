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
token(let, let) :- !.

% Identifiers
token(Id, ident(Id)).
