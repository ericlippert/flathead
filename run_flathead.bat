del deque.cmi
del deque.cmo
ocamlc -c deque.ml
del screen.cmi
del screen.cmo
ocamlc -c screen.ml
ocaml deque.cmo screen.cmo graphics.cma flathead.ml
