% Helper predicates
:- module(parser_helper, [obj_type_list/3]).

% obj_type_list(+NameList, +Type, -ObjTypeList).
% ObjTypeList is the list of terms obj(Name, Type).
obj_type_list([], _, []).
obj_type_list([NameHead | NameTail], Type, [obj(NameHead, Type) | ObjTypeTail])
:- obj_type_list(NameTail, Type, ObjTypeTail).


