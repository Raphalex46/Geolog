# Geolog
Geolog is a description language for geometric construction problems written in Prolog. It aims to look like natural language while keeping a formal structure.

## Requirements

In order for Geolog to run, you need to have a prolog interpreter installed.
SWI-Prolog is recommended, because the compiler was written for, and tester with
SWI-Prolog.

## Usage

With the `swipl` command, the entry-point file of the program is `src/run.pl`,
so simply run: `swipl src/run.pl [OPTIONS] <input_file>`

Or simply use the wrapper script: `./run [OPTIONS] <input_file>`

## Available options

Run `./run --help` to have a list of the available options

## Implementing a natural language module

Natural language modules are placed in the `src/langs` directory.

In this directory, create a directory named after the language you want to 
implement (for exemple, `src/langs/french`).

The language needs to implement a number of
[Prolog modules](https://www.swi-prolog.org/pldoc/man?section=modules),
each one in a separate file.
The following files need to be present in the directory of the language to
implement: 
- `decl.pl`
- `relation.pl`
- `goal_decl.pl`
- `typename.pl`

Each one is a separate module, and they respectively need to implement the
following predicates:

- in `decl.pl`:
  - `decl/3`
- in `relation.pl`:
  - `relation/5`
- in `goal_decl.pl`:
  - `goal_decl/4`
- in `typename.pl`:
  - `typename/3`

Each one of those predicates is a
[DCG](https://www.swi-prolog.org/pldoc/man?section=DCG).

For an example, see the already implemented language modules, such as `french`
and `english`.

## Implementing a backend

Backends in this context refer to the module that translates the intermediate
language generated by the core compiler (with the natural language modules
loaded).
Similarly to the language modules, the backend modules (or translators) are
located in the `translators` directory with a Prolog file named after the
translator.

The translator module needs to implement only the `translate/4` predicate
(not a DCG).

For an example, see the already implemented backend module for Proge.
