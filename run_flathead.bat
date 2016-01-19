del deque.cmi
del deque.cmo
ocamlc -c deque.ml
del screen.cmi
del screen.cmo
ocamlc -c screen.ml
del immutable_bytes.cmi
del immutable_bytes.cmo
ocamlc -c immutable_bytes.ml
del memory.cmi
del memory.cmo
ocamlc -c memory.ml
ocaml deque.cmo screen.cmo immutable_bytes.cmo memory.cmo graphics.cma flathead.ml
