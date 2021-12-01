:- module(lexer, [lex/2]).
:- use_module(tokens).

% lex(Input, TokenList).
% "public" predicate that transforms a list of atoms representing the input
% program into a list of tokens for the parsers.
lex([], []).
lex([InputHead | InputTail], [TokenHead | TokenTail]) :-
  % Get the token associated with the input atom.
  % We use a cut because there should only be one solution
  token(InputHead, TokenHead), !,
  % Recursive call
  lex(InputTail, TokenTail).
