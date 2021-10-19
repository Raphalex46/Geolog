:- use_module(library(porter_stem)).

:- use_module(load).
:- use_module(lexer).
:- use_module(parser).

  % Program entry point.
:-
  parse_command_line(Opts, PositionalArgs),
  
  % Assert all options. They can be accessed with load:option/1
  assert_options(Opts).

run(Input) :-
  % Use the tokenize_atom predicate from the porter_stem library to cut the
  % input string into atoms
  tokenize_atom(Input, AtomList),
  % Run lexer
  print(AtomList), nl,
  % Call the lexer
  lex(AtomList, TokenList),
  % Debug print for testing the lexer.
  print('Tokenized input: '), nl,
  print(TokenList), nl,

  % Pass the list of tokens to the parser.
  phrase(parse(DeclList, _, _), TokenList),
  print('Parsed result: '), nl,
  print(DeclList), nl.
