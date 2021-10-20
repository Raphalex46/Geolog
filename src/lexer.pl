:- module(lexer, [lex/2]).
:- use_module(tokens).
:- use_module(i18n).

% lex(Input, TokenList).
% "public" predicate that transforms a list of atoms representing the input
% program into a list of tokens for the parsers.
lex([], []).
lex([InputHead | InputTail], [TokenHead | TokenTail]) :-
  % Get chosen language
  load:option(lang(Lang)),
  % Translate input atom with the key
  (
    % If the translation predicate fails, that means that the element is
    % a keyword, a type name or a relation name, and doesn't have any
    % valid traduction. That means that there is a "language conflict" 
    % (for example calling an object "let" in french mode). If we are
    % in such a situation, juste always consider that this is an identifier.
    i18n:i18n(Lang, Atom, InputHead),
    token(Atom, TokenHead)
  ; TokenHead = ident(InputHead)
  ),
  % Get token corresponding to atom
  % Recursive call
  lex(InputTail, TokenTail).
