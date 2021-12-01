:- module(typename_french, [typename/3, plural_typename/3]).

% Points
typename(point) --> [point].
typename(point) --> [un, point].
typename(point) --> [le, point].

% Lines
typename(line) --> [droite].
typename(line) --> [une, droite].
typename(line) --> [la, droite].

% Plural version for the typenames
plural_typename(point) --> [des, points].
plural_typename(point) --> [les, points].

plural_typename(line) --> [des, droites].
plural_typename(line) --> [les, droites].
