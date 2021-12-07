% This file describes the Proge translator
:- use_module('../parser_helper').

% Predicate to translate our 'intermediary language' into 
% something that can be understood by a solving engine (in this
% case Proge).
translate(DeclList, ConsList, GoalList, Translation) :-
  length(DeclList, DeclLength),
  % Again, cut is used because the first solution will be good
  translate_decl(DeclLength, DeclList, DeclTranslation, donne), !,
  % Transform the goals list into a declaration list so we can use the
  % same predicate for both declarations and goals
  goal_list_to_obj_list(GoalList, ObjList),
  length(ObjList, ObjLength),
  GoalLength is ObjLength + DeclLength,
  translate_decl(GoalLength, ObjList, GoalTranslation, cherche), !,
  % Translate the constraints
  length(ConsList, ConsLength),
  translate_cons(ConsLength, ConsList, ConsTranslation), !,
  % Project the goal list to the constraints expressed in each goal and
  % call the translate_cons predicate again on those.
  goal_list_to_cons_list(GoalList, GoalConsList),
  length(GoalConsList, GoalConsListLength),
  GoalConsLength is GoalConsListLength + ConsLength,
  translate_cons(GoalConsLength, GoalConsList, GoalConsTranslation), !,
  % Concatenate all of the translations
  string_concat(DeclTranslation, GoalTranslation, AllDeclTranslation),
  string_concat(ConsTranslation, GoalConsTranslation, AllConsTranslation),
  string_concat(AllDeclTranslation, AllConsTranslation, Translation).

% Translate declarations
% translate_decl(NumberOfDeclarations, DeclList, Translation, DeclType)
translate_decl(_, [], '', _).
translate_decl(N, [obj(Name, Type) | DeclTail], Translation, DeclType) :-
  length(DeclTail, ListLength),
  % Compute index of current declaration
  Index is N - (ListLength + 1), 
  % Actually translate the declaration
  translate_type_name(Type, ProgeType),
  % Make the name lowercase (since Proge uses prolog directly)
  downcase_atom(Name, DownName),
  atomics_to_string([Index, ' \'dec:\' ', ProgeType, ' :: ', DownName, ' ',
  DeclType, '.\n'],
  InterTranslation),
  % Recursive call followed by string concatenation
  translate_decl(N, DeclTail, NextTranslation, DeclType),
  string_concat(InterTranslation, NextTranslation, Translation).

% Translate constraints
% translate_cons(NumberOfConstraints, ConsList, Translation)
translate_cons(_, [], '').
translate_cons(N, [cons(ObjA, Rel, ObjB) | ConsTail], Translation) :-
  length(ConsTail, ListLength),
  % Compute index of current constraint
  Index is N - (ListLength + 1),
  % Actually translate the constraint
  translate_relation(ObjA, Rel, ObjB, TransRel),
  % Transform it to downcase
  downcase_atom(TransRel, DownTransRel),
  atomics_to_string([Index, ' \'cont:\' ', DownTransRel, '.\n'],
  InterTranslation),
  % Recursive call followed by string concatenation
  translate_cons(N, ConsTail, NextTranslation),
  string_concat(InterTranslation, NextTranslation, Translation).

% Predicate to convert the goals list to an obj list (like the declaration list)
% It's essentally a projection of the goals list to the declarations
% This is more friendly to the Proge file format.
goal_list_to_obj_list([], []).
goal_list_to_obj_list([goal(NameList, Type, _) | GoalTail], ObjList) :-
  obj_type_list(NameList, Type, ObjHead),
  goal_list_to_obj_list(GoalTail, ObjTail),
  append(ObjHead, ObjTail, ObjList).

% Predicate to convert the goals list to a list of constraints.
% It's essentially a projection of the goals list to the constraints
goal_list_to_cons_list([], []).
goal_list_to_cons_list([goal(_, _, GoalConsList) | GoalTail], ConsList) :-
  goal_list_to_cons_list(GoalTail, ConsTail),
  append(GoalConsList, ConsTail, ConsList).

% translate_type_name(Type, ProgeType)

% Use cuts for each 'defined' translation so we can have
% a general translation so unknown types just 'go through'
translate_type_name(line, droite) :- !.
translate_type_name(circle, cercle) :- !.
translate_type_name(Type, Type).

% translate_relation(ObjA, Rel, ObjB, TransRel)

% Same thing with the cuts

translate_relation(ident(A), on, ident(B), Translation) :-
  atomic_list_concat([A, 'est_sur', B], ' ', Translation).
  
translate_relation(ident(A), middle, segment(B, C), Translation)
:-
  atomic_list_concat([A, '\'=p=\'', 'mil(', B, ',', C, ')'], ' ', Translation).

translate_relation(ident(A), ccc, triangle(B, C, D), Translation) :-
  atomic_list_concat(['ccc(', B, ', ', C, ', ', D, ')'], Constructor),
  atomic_list_concat([A, '\'=p=\'', Constructor], ' ', Translation).

translate_relation(distance(A, B), eq, distance(C, D), Translation) :-
  atomic_list_concat(['dist(', A, ', ', B, ')'], DistA),
  atomic_list_concat(['dist(', C, ', ', D, ')'], DistB),
  atomic_list_concat([DistA, ' \'=l=\' ', DistB], Translation).
  
% General no-translation case
translate_relation(_, Rel, _, Rel) :-
  write('Unknown relation').
