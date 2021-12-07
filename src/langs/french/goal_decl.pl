% This file describes the way to declare a goal in french
:- module(goal_decl, [goal_decl/4]).
:- use_module(src/langs/french/typename).
:- use_module(src/langs/french/name).

goal_decl(NameList, Type) --> goal_header, name_list(NameList), optional([de,
                              type]), typename(Type), such_that.

% Singular
goal_decl([Name], Type) --> goal_header, typename(Type), [ident(Name)],
                            such_that.

% Plural
goal_decl(NameList, Type) --> goal_header, plural_typename(Type),
                              name_list(NameList), such_that.

goal_header --> [trouver].
goal_header --> [construire].
goal_header --> [déterminer].

such_that --> [tel, que].
such_that --> [tels, que].
such_that --> [telle, que].
such_that --> [de, sorte, que].
