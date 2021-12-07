:- module(typename_french, [typename/3, plural_typename/3, generic_typename/3]).
:- use_module(src/parser_helper).

% For syntaxic constructions that don't care about plural or singular
generic_typename(Type) --> typename(Type).
generic_typename(Type) --> plural_typename(Type).

% Singular
%
% Points
typename(point) --> article, [point].

% Lines
typename(line) --> article, [droite].

% Circles
typename(circle) --> article, [cercle].

% Plural

% Points
plural_typename(point) --> plural_article, [points].

% Lines
plural_typename(line) --> plural_article, [droites].

% Circles
plural_typename(circle) --> plural_article, [cercles].

% Little symbols for articles
article --> choice([[la], [le], [un], [une], []]).
plural_article --> choice([[les], [des], []]).

