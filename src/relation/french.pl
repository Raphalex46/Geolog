% This file describes relations in french

:- module(relation_french, [relation/5]).
:- use_module(src/obj/french).

relation(ObjA, middle, ObjB) -->
  obj(ObjA), [est, milieu, de], obj(ObjB).

relation(ObjA, on, ObjB) -->
  obj(ObjA), [est, sur], obj(ObjB).

relation(ObjA, on, ObjB) -->
  obj(ObjB), [contient], obj(ObjA).
