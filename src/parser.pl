:- module(parser, [parse/5, comp_error/0, comp_error_message/1]).
:- use_module(parser_helper).
:- use_module(tokens).
:- use_module(load).
:- use_module(type).
:- use_module(relation).

% This predicate is used to indicate wether a fatal compilation error happened.
% This allows clean error prints (the output isn't flodded with similar
% error messages that are printed when Prolog backtracks). When an error
% happens, the parser asserts this predicate, which is then used by the
% calling predicate to throw away the result of the compilation.

:- dynamic comp_error/0.
:- dynamic comp_error_message/1.

% Load correct language modules
load_modules :-
  option(lang(Lang)),
  use_module(src/langs/Lang/decl),
  use_module(src/langs/Lang/relation),
  use_module(src/langs/Lang/goal_decl),
  use_module(src/langs/Lang/typename).

% For each module, check that it implements each type defined in the
% corpus
check_modules :-
  bagof(Type, type(Type), TypeBag),
  check_type_module(TypeBag),
  bagof(Rel, relation:relation(Rel), RelBag),
  check_relation_module(RelBag).
  
% Check if there is a symbol for each type in the selected language
check_type_module([]).
check_type_module([TypeHead | TypeTail]) :-
  (
    \+ typename(TypeHead, _, _)
  -> option(lang(Lang)),
    atomics_to_string(['The language module \'', Lang, '\' does not implement\c
    the type \'', TypeHead, '\'.'], ErrStr),
    write(ErrStr), nl, assert(comp_error)
  ; true
  ),
  check_type_module(TypeTail).

% Same thing for relations
check_relation_module([]).
check_relation_module([RelHead | RelTail]) :-
  (
    \+ relation(_, RelHead, _, _, _)
  -> option(lang(Lang)),
    atomics_to_string(['The language module \'', Lang, '\' does not implement \c 
    the relation \'', RelHead, '\'.'], ErrStr),
    write(ErrStr), nl, assert(comp_error)
  ; true
  ),
  check_relation_module(RelTail).


% We associate a prolog term to each syntax element. 
%
% obj(Name, Type) -> declared object
% cons(ObjA, Rel, ObjB) -> constraint between object A and object B (relation)
% goal(NameList, Type, ConsList) -> objects to find, with a type, and attached
% to a list of constraints

% Entry symbol. The program is made of a declarations list, a constraint list
% and a goal list

% We flatten the declarations list to have a nice list of objects with
% their associated types.
parse(FlattenedDeclList, ConsList, GoalList) -->
% Load modules corresponding to the selected language
  { load_modules, check_modules },
  (decl_list(DeclList); {assert(comp_error_message('An error occured while \c
    parsing a declaration'))}), {flatten(DeclList, FlattenedDeclList)},
  (cons_list(ConsList); {assert(comp_error_message('An error occured while \c
    parsing a constraint'))}),
  (goal_list(GoalList); {assert(comp_error_message('An error occured while \c
    parsing a goal'))}).

% Constraints between two objects.
% This predicate just calls the language-specific relation predicate.
cons(cons(ObjA, Rel, ObjB)) -->
  relation(ObjA, Rel, ObjB),
  % Check that the relation exists
  {(\+ relation:relation(Rel) -> write('Unknown relation \''), write(Rel),
  write('\''), nl, assert(comp_error); true)}.

% Goals (objects to find)
% This predicate calls the language-specific goal_decl predicate followed
% by a list of constraints.
% A 'cons' is added, since we want at least one constraint.
% (Since cons_list allows empty lists).
goal(goal(NameList, Type, [ConsHead | ConsList])) -->
  goal_decl(NameList, Type),
  % Check if the type exists
  {(\+ type(Type) -> write('Unknown type \''), write(Type), write('\''), nl,
  assert(comp_error); true)},
  cons(ConsHead), [punctuation('.')], cons_list(ConsList).

% List of declarations (separated by '.').
% This predicate calls the language-specific decl predicate.
% Base case: no declaration
decl_list([]) --> [].
% Recursive case
decl_list([DeclHead | DeclTail]) -->
  decl(DeclHead), 
  % Deconstruct the declaration and check if the type exists
  {DeclHead = [obj(_, Type)|_], (\+ type(Type) -> write('Unknown type \''),
  write(Type), write('\''), nl, assert(comp_error); true)}, [punctuation('.')],
  decl_list(DeclTail).

% List of constraints
cons_list([]) --> [].
cons_list([ConsHead | ConsTail]) -->
  cons(ConsHead), [punctuation('.')], cons_list(ConsTail).

% List of goals
goal_list([]) --> [].
goal_list([GoalHead | GoalTail]) -->
  goal(GoalHead), goal_list(GoalTail).
