:- use_module(load).
:- use_module(lexer).

:-
  % Program entry point.
  parse_command_line(Opts, PositionalArgs),
  
  % Assert all options. They can be accessed with load:option/1
  assert_options(Opts).
