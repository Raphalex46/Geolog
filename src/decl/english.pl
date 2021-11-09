:- module(decl_english, [decl/3]).

:- use_module('src/parser_helper').
:- use_module('src/typename/english').
:- use_module('src/name/english').

% Declaration symbol
decl(DeclList) --> [let], name_list(NameList), [be, of, type],
typename(TypeName), {obj_type_list(NameList, TypeName, DeclList)}.
