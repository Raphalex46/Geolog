% This file describes the way to declare objects in french
:- module(decl_french, [decl/3]).

:- use_module(src/typename/french).
:- use_module(src/parser_helper).
:- use_module(src/name/french).

decl(DeclList) -->
  [soit], name_list(NameList), [de, type], typename(Typename),
  {obj_type_list(NameList, Typename, DeclList)}.

