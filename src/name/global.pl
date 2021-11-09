% Global file that can be used by language files

name_list([Name]) --> [ident(Name)].
name_list([NameHead | NameTail]) -->
  [ident(NameHead), punctuation(',')], name_list(NameTail).
