:- module(tokens, [token/2]).
:- use_module(load).
:- use_module(relation).

% token(Atom, Token).
% The token predicate converts input tokens into tokens for the parser

% Keywords
% This predicate is used to distinguish between keywords and identifiers
keyword(let).
keyword(of).
keyword(type).
keyword('is').

% Ponctuation
punctuation(',').
punctuation('.').

% Type names
type_name(point).
type_name(line).
type_name(circle).

% Actual token predicates Cuts are used because technically, an input atom can
% only be associated to one token, so there is no need to search the rest of
% the tree. These are green cuts I think (as long as there is no X such that
% out of keyword(X), punctuation(X) and type_name(X), no two are provable at
% the same time.

% Keywords
token(Kw, keyword(Kw)) :- keyword(Kw), !.

% Punctuation
token(P, punctuation(P)) :- punctuation(P), !.

% Type names
token(X, type_name(X)) :- type_name(X), !.

% Relations
token(Rel, relation(Before, Rel, After)) :- relation(Before, Rel, After), !.

% Identifiers
token(Id, ident(Id)) :-
  % Id is an identifier only if it isn't also a keyword (otherwise it's a
  % keyword)
  \+ keyword(Id),
  \+ punctuation(Id),
  \+ type_name(Id),
  \+ relation(_, Id, _),
  !.
