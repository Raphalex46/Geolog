:- module(parser, [parse/5]).
:- use_module(parser_helper).
:- use_module(tokens).

% We associate a prolog term to each syntax element. 
%
% obj(Name, Type) -> declared object
%

% Entry symbol. The program is made of a declarations list, a constraint list
% and a goals list.
parse(DeclList, ConsList, GoalList) -->
  decl_list(DeclList).%,cons_list(ConsList), goal_list(GoalList).

% Object declaration statement. The user can declare multiple objects (names
% are contained in NameList
decl(ObjList) -->
  [keyword(let)], name_list(NameList), [keyword(of)], [keyword(type)],
  [type_name(Type)], {obj_type_list(NameList, Type, ObjList)}, !.


% List versions (meaning, mutiple syntax elements (multiple declarations,
% multiple constraints

% The cuts are used because, when the entire recursive sequence has been found,
% there is no need to backtrack (we don't want other interpretations anyway).
% I think those cuts are red.

% List of declarations (separated by '.').

% Base case: no declaration
decl_list([]) --> [].
decl_list([DeclHead | DeclTail]) -->
  decl(DeclHead), [punctuation('.')], decl_list(DeclTail).

% List of identifiers (comma separated)
name_list([Name]) --> [ident(Name)].
name_list([NameHead | NameTail]) -->
  [ident(NameHead), punctuation(',')], name_list(NameTail).
