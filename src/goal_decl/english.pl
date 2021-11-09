:- module(goal_decl_english, [goal_decl/4]).
:- use_module(src/typename/english).
:- use_module(src/name/english).

goal_decl(NameList, Type) -->
  [find], name_list(NameList), [of, type], typename(Type), [such, that].
