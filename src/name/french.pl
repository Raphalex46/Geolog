:- module(name_french, [name_list/3]).
:- consult(src/name/global).
:- multifile name_french:name_list/3.

name_list([Name]) --> [ident(Name)].
name_list([NameHead | NameTail]) --> [ident(NameHead), et], name_list(NameTail).
