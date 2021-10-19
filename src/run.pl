:- use_module(load).
:- use_module(lexer).
:- use_module(library(porter_stem)).

run(Input) :-
  % Program entry point.
  parse_command_line(Opts, PositionalArgs),
  
  % Assert all options. They can be accessed with load:option/1
  assert_options(Opts),

  % Use the tokenize_atom predicate from the porter_stem library to cut the input string
  % into atoms
  tokenize_atom(Input, AtomList),
  % Run lexer
  print(AtomList), nl,
  % We use a "red" cut here to only keep the first solution. if the lexer is
  % correct (which I hope it is), every solution should be identical anyway, so
  % this cut just saves some computation time
  lex(AtomList, TokenList), !,
  % Debug print for testing the lexer.
  print(TokenList).
