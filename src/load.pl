:- module(load, [parse_command_line/2, assert_options/1, option/1, version/1]).
:- use_module(library(optparse)).

% version(Version).
version('0.5.2').

% option(Option).
:- dynamic option/1.

% List of available options
optspecs(
  [
    [
      % Name of output file
      opt(output_file),
      type(atom),
      shortflags([o]),
      longflags([output]),
      default(out),
      help(['output file name'])
    ],
    [
      % Language selection
      opt(lang),
      type(atom),
      shortflags([l]),
      longflags([lang]),
      default(english),
      help('Natural language selection')
    ],
    [
      opt(translator),
      type(atom),
      shortflags([t]),
      longflags([translator]),
      default(proge),
      help('Translator selection')
    ],
    [
      opt(interactive),
      type(boolean),
      shortflags([i]),
      longflags([it]),
      default(false),
      help('Enter the prolog command line after execution (for testing)')
    ],
    [
      opt(version),
      type(boolean),
      shortflags([v]),
      longflags([version]),
      default(false),
      help('Print version number and exit')
    ],
    [
      opt(help),
      type(boolean),
      shortflags([h]),
      longflags([help]),
      default(false),
      help('Print this help text and exit')
    ]
  ]
).

% parse_command_line(Opts, PositionalArgs).
% Parse the command line to get a list of options and positional arguments
parse_command_line(Opts, PositionalArgs) :-
  % Get our option specification and the cli input (argv)
  optspecs(OptSpec),
  current_prolog_flag(argv, Argv),
  opt_parse(OptSpec, Argv, Opts, PositionalArgs).

% assert_options(Opts).
% This predicate asserts all options passed in the command line
% as terms of the form option(Opt)
assert_options([]).
assert_options([HeadOpt | Opts]) :-
  assertz(option(HeadOpt)),
  assert_options(Opts).
