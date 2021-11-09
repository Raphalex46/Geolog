:- module(tokens, [token/2]).
:- use_module(load).
:- use_module(relation).

% token(Atom, Token).
% The token predicate converts input tokens into tokens for the parser

% Keywords
% This predicate is used to distinguish between keywords and identifiers
% This predicate indicates that every atom that is not fully upper case is
% a keyword.
keyword(Atom) :- \+ upcase_atom(Atom, Atom).

% Ponctuation
% Special predicate for the punctuation characters
punctuation(',').
punctuation('.').

% Actual token predicates Cuts are used because technically, an input atom can
% only be associated to one token, so there is no need to search the rest of
% the tree. These are green cuts I think (as long as there is no X such that
% out of keyword(X), punctuation(X) and type_name(X), no two are provable at
% the same time.

% Keywords
token(Kw, Kwl) :- keyword(Kw), downcase_atom(Kw, Kwl).

% Punctuation
token(P, punctuation(P)) :- punctuation(P).

% Identifiers
% An atom is an identifier if it isn't punctuation and isn't a keyword
% (all identifiers are fully uppercase
token(Id, ident(Id)) :- \+ punctuation(Id), upcase_atom(Id, Id).
