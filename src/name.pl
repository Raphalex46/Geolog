% Global file that can be used by language files
:- multifile name_list/3.

name_list([Name]) --> [ident(Name)].
name_list([NameHead | NameTail]) -->
  [ident(NameHead), punctuation(',')], name_list(NameTail).
