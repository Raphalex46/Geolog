% This file lists ways to declare goals in the english language setting
:- module(goal_decl_english, [goal_decl/4]).
:- use_module(src/langs/english/typename).
:- use_module(src/langs/english/name).

goal_decl(NameList, Type) -->
  [find], name_list(NameList), [of, type], typename(Type), [such, that].
