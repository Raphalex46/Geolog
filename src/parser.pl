:- module(parser, [parse/5]).
:- use_module(parser_helper).
:- use_module(tokens).

% We associate a prolog term to each syntax element. 
%
% obj(Name, Type) -> declared object
% cons(ObjA, Rel, ObjB) -> constraint between object A and object B (relation)

% Entry symbol. The program is made of a declarations list, a constraint list
% and a goals list.
parse(DeclList, ConsList, GoalList) -->
  decl_list(DeclList), cons_list(ConsList).%, goal_list(GoalList).

% Object declaration statement. The user can declare multiple objects (names
% are contained in NameList
decl(ObjList) -->
  [keyword(let)], name_list(NameList), [keyword(of)], [keyword(type)],
  [type_name(Type)], {obj_type_list(NameList, Type, ObjList)}.

% Constraints between two objects. Type checking is done at a later stage.
cons(cons(ObjA, Rel, ObjB)) -->
  obj(ObjA), [keyword(Before)], [relation(Before, Rel, After)],
  [keyword(After)], obj(ObjB).

% Case where there is no keyword after the relation
cons(cons(ObjA, Rel, ObjB)) -->
  obj(ObjA), [keyword(Before)], [relation(Before, Rel, nil)], obj(ObjB).

% Object reference. The object can be the name of a declared object or
% a call to an object constructor.
obj(Name) --> [ident(Name)].
% TODO: add other cases (constructors)

% List versions (meaning, mutiple syntax elements (multiple declarations,
% multiple constraints

% List of declarations (separated by '.').
% Base case: no declaration
decl_list([]) --> [].
% Recursive case
decl_list([DeclHead | DeclTail]) -->
  decl(DeclHead), [punctuation('.')], decl_list(DeclTail).

% List of constraints
cons_list([]) --> [].
cons_list([ConsHead | ConsTail]) -->
  cons(ConsHead), [punctuation('.')], cons_list(ConsTail).

% List of identifiers (comma separated)
name_list([Name]) --> [ident(Name)].
name_list([NameHead | NameTail]) -->
  [ident(NameHead), punctuation(',')], name_list(NameTail).
