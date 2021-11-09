:- module(parser, [parse/5]).
:- use_module(parser_helper).
:- use_module(tokens).
:- use_module(load).

% Load correct language modules
load_modules :-
  option(lang(Lang)),
  use_module(src/langs/Lang/decl),
  use_module(src/langs/Lang/relation),
  use_module(src/langs/Lang/goal_decl).

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
  { load_modules },
  decl_list(DeclList), {flatten(DeclList, FlattenedDeclList)},
  cons_list(ConsList), goal_list(GoalList).

% Constraints between two objects.
% This predicate just calls the language-specific relation predicate.
cons(cons(ObjA, Rel, ObjB)) --> relation(ObjA, Rel, ObjB).

% Goals (objects to find)
% This predicate calls the language-specific goal_decl predicate followed
% by a list of constraints.
% A 'cons' is added, since we want at least one constraint.
% (Since cons_list allows empty lists).
goal(goal(NameList, Type, [ConsHead | ConsList])) -->
  goal_decl(NameList, Type), cons(ConsHead), [punctuation('.')], cons_list(ConsList).

% List of declarations (separated by '.').
% This predicate calls the language-specific decl predicate.
% Base case: no declaration
decl_list([]) --> [].
% Recursive case
decl_list([DeclHead | DeclTail]) -->
  decl(DeclHead), [punctuation('.')], decl_list(DeclTail).

% List of constraints
cons_list([]) --> [].
cons_list([ConsHead | ConsTail]) -->
  cons(ConsHead), [punctuation('.')], cons_list(ConsTail).

% List of goals
goal_list([]) --> [].
goal_list([GoalHead | GoalTail]) -->
  goal(GoalHead), goal_list(GoalTail).
