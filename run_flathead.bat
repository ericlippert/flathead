@del *.cmi
@del *.cmo
ocamlc -g -c type.ml
ocamlc -g -c utility.ml
ocamlc -g -c immutable_bytes.ml
ocamlc -g -c story.ml
ocaml type.cmo utility.cmo immutable_bytes.cmo story.cmo flathead.ml
@del *.cmi
@del *.cmo
