% This file describes the different ways to declare objects in the english
% language setting
:- module(decl_english, [decl/3]).

:- use_module('src/parser_helper').
:- use_module('src/langs/english/typename').
:- use_module('src/langs/english/name').

% Declaration symbol
decl(DeclList) --> [let], name_list(NameList), [be, of, type],
typename(Typename), {obj_type_list(NameList, Typename, DeclList)}.
