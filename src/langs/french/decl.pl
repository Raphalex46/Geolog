% This file describes the way to declare objects in french
:- module(decl_french, [decl/3]).

:- use_module(src/langs/french/typename).
:- use_module(src/parser_helper).
:- use_module(src/langs/french/name).

% More formal declaratin
decl(DeclList) -->
  decl_header, name_list(NameList), [de, type], typename(Typename),
  {obj_type_list(NameList, Typename, DeclList)}.

% Declaration with subject first

% Singular version
decl([obj(Name, Type)]) -->
  [ident(Name), est], typename(Type).

% Plural version
% (This version checks that the list isn't of size 1 as well)
decl(DeclList) -->
  name_list(NameList), {length(NameList, L), L > 1}, [sont], plural_typename(Typename),
  {obj_type_list(NameList, Typename, DeclList)}.

% Declaration with type first

% Singular version
decl([obj(Name, Type)]) -->
  decl_header, typename(Type), optional([nommé]), [ident(Name)].
decl(DeclList) -->
  decl_header, plural_typename(Typename), optional([nommés]),
  name_list(NameList), {length(NameList, L), L > 1}, {obj_type_list(NameList,
  Typename, DeclList)}.

% Declaration type headers
decl_header --> [soit].
decl_header --> [on, considère].
decl_header --> [on, nomme].
