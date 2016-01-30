@del *.cmi
@del *.cmo
ocamlc -g -c type.ml
ocamlc -g -c utility.ml
ocamlc -g -c immutable_bytes.ml
ocamlc -g -c story.ml
ocamlc -g -c zstring.ml
ocaml type.cmo utility.cmo immutable_bytes.cmo story.cmo zstring.cmo flathead.ml
@del *.cmi
@del *.cmo
