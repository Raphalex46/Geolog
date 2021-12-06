% Global file defining object symbols that do not depend on the language

obj(ident(Name)) --> [ident(Name)].
obj(segment(A, B)) --> [punctuation('['), ident(A), ident(B), punctuation(']')].
obj(triangle(A, B, C)) --> [ident(A), ident(B), ident(C)].