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
  % Like below, the cut is used to keep only the first correct solution
  lex(AtomList, TokenList), !,
  % Debug print for testing the lexer.
  print('Tokenized input: '), nl,
  print(TokenList), nl,

  % Pass the list of tokens to the parser.
  % The cut is used to keep only the first correct solution (we don't need
  % other solutions)
  phrase(parse(DeclList, ConsList, _), TokenList), !,
  print('Parsed result: '), nl,
  print('Declarations list: '), nl,
  print(DeclList), nl,
  print('Constraints list: '), nl,
  print(ConsList).
