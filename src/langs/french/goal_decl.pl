% This file describes the way to declare a goal in french
:- module(goal_decl, [goal_decl/4]).
:- use_module(src/langs/french/typename).
:- use_module(src/langs/french/name).

goal_decl(NameList, Type) --> [trouver], name_list(NameList), [de, type],
typename(Type), [tel, que].
