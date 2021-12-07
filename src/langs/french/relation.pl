% This file describes relations in french

:- module(relation_french, [relation/5]).
:- use_module(src/langs/french/obj).

relation(ObjA, middle, ObjB) -->
  obj(ObjA), [est, milieu, de], obj(ObjB).
relation(ObjA, middle, ObjB) -->
  obj(ObjA), [est, le, milieu, de], obj(ObjB).

relation(ObjA, on, ObjB) -->
  obj(ObjA), [est, sur], obj(ObjB).

relation(ObjA, on, ObjB) -->
  obj(ObjB), [contient], obj(ObjA).

relation(ObjA, on, ObjB) -->
  obj(ObjB), [passe, par], obj(ObjA).
  
relation(ObjA, ccc, ObjB) -->
  obj(ObjA), [est, le, centre, du, cercle, circonscrit, à], obj(ObjB).

relation(ObjA, eq, ObjB) -->
  obj(ObjA), [punctuation('=')], obj(ObjB).
