:- module(tokens, [token/2]).
:- use_module(load).

% token(Atom, Token).
% The token predicate converts input tokens into tokens for the parser

% Keywords
% This predicate is used to distinguish between keywords and identifiers
% This predicate indicates that every atom that is not fully upper case is
% a keyword.
keyword(Atom) :- \+ upcase_atom(Atom, Atom).

% Punctuation
% Special predicate for the punctuation characters
punctuation(',').
punctuation('.').

% Keywords
% The tokenization process actually transforms all keywords to lowercase for
% simplicity
token(Kw, Kwl) :- keyword(Kw), downcase_atom(Kw, Kwl).

% Punctuation
token(P, punctuation(P)) :- punctuation(P).

% Identifiers
% An atom is an identifier if it isn't punctuation and isn't a keyword
% (all identifiers are fully uppercase
token(Id, ident(Id)) :- \+ punctuation(Id), upcase_atom(Id, Id).
