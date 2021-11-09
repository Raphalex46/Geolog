:- module(typename_french, [typename/3]).

% Points
typename(point) --> [point].
typename(point) --> [un, point].

% Lines
typename(line) --> [droite].
typename(line) --> [une, droite].

