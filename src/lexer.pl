:- module(lexer, [lex/2]).
:- use_module(tokens).

% lex(Input, TokenList).
% "public" predicate that transforms a list of atoms representing the input
% program into a list of tokens for the parsers.
lex([], []).
lex([InputHead | InputTail], [TokenHead | TokenTail]) :-
  % Get chosen language
  load:option(lang(Lang)),
  % Translate input atom with the key
  i18n(Lang, Atom, InputHead),
  % Get token corresponding to atom
  token(Atom, TokenHead),
  % Recursive call
  lex(InputTail, TokenTail).
