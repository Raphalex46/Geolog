% Module that describes relations
:- module(relation_english, [relation/5]).
:- use_module(src/langs/english/obj).

relation(ObjA, middle, ObjB) -->
  obj(ObjA), ['is', the, middle, of], obj(ObjB).

relation(ObjA, on, ObjB) -->
  obj(ObjA), ['is', on], obj(ObjB).

relation(ObjA, on, ObjB) -->
  obj(ObjB), [contains], obj(ObjA).
