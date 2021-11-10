% Helper predicates
:- module(parser_helper, [obj_type_list/3, choice/3, optional/3]).

% obj_type_list(+NameList, +Type, -ObjTypeList).
% ObjTypeList is the list of terms obj(Name, Type).
obj_type_list([], _, []).
obj_type_list([NameHead | NameTail], Type, [obj(NameHead, Type) | ObjTypeTail])
:- obj_type_list(NameTail, Type, ObjTypeTail).

% choice(SymbolList)
% Given a list of symbols (terminal or not), this is an easy way to write
% a disjonction of all cases

% Base case
choice([]) --> {fail}.
% Recursive case
choice([SymbolHead | _]) --> SymbolHead.
choice([_ | SymbolTail]) --> choice(SymbolTail).

% optional(Symbol)
% Given a symbol, this rule expresses that this symbol can be found but is
% optional.

optional(_) --> {true}.
optional(Symbol) --> Symbol.
