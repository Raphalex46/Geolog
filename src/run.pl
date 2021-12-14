:- use_module(library(porter_stem)).
:- use_module(library(optparse)).
:- use_module(library(readutil)).

:- use_module(load).
:- use_module(lexer).
:- use_module(parser).

% Program entry point.
main :-
  parse_command_line(Opts, PositionalArgs),
  
  % Assert all options. They can be accessed with load:option/1
  assert_options(Opts),

  % Check version and help options

  % First, get optspecs
  load:optspecs(OptSpecs),
  (
    % Check for the help option
    option(help(true)) 
  ->
    opt_help(OptSpecs, Help),
    write(Help),
    halt
  ;
    option(version(true))
  ->
    print_version_and_halt
  ;

    % Load the translator
    option(translator(Translator)),
    atom_concat('src/translators/', Translator, TranslatorFile),
    consult(TranslatorFile),

    % Check if there is an incorrect number of positional arguments
    (\+ length(PositionalArgs, 1)
    -> print_usage_and_halt
    ;
    % Finally, load the input file and compile it
    PositionalArgs = [Filename],
    read_file_to_string(Filename, InputString, []),
    (
      run(InputString, FinalTranslation),
      % Check if there was a compilation error
      \+ comp_error,
      write(FinalTranslation)
    ; write('Compilation error. Aborting.'), nl,
      % Get all of the compile error messages and display them
      % setof(Message, comp_error_message(Message), MessageSet),
      comp_error_message(Message),
      write(Message), nl
    ),
    % Check for interactive mode and exit if it is false
    (
      option(interactive(false)) ->
      halt
    ;
      true
    )
  )
).


run(Input, FinalTranslation) :-
  % Use the tokenize_atom predicate from the porter_stem library to cut the
  % input string into atoms
  tokenize_atom(Input, AtomList),
  % Call the lexer
  % Like below, the cut is used to keep only the first correct solution
  lex(AtomList, TokenList), !,
  % Pass the list of tokens to the parser.
  % The cut is used to keep only the first correct solution (we don't need
  % other solutions)
  (
    phrase(parse(DeclList, ConsList, GoalList), TokenList), !
  ;
    assert(comp_error)
  ),
  translate(DeclList, ConsList, GoalList, FinalTranslation), !.

% Utility predicate that does what it says
print_usage_and_halt :-
  write('Usage: geolog [options] <input_file>'), nl, halt.

% Utility predicate that does what it says
print_version_and_halt :-
  version(Version),
  atom_concat('Geolog version ', Version, VersionPrint),
  write(VersionPrint), nl,
  halt.

% Actually call the 'main' predicate
:- main.
