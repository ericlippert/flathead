@del *.cmi
@del *.cmo
ocamlc -g -c type.ml
ocamlc -g -c utility.ml
ocamlc -g -c immutable_bytes.ml
ocamlc -g -c story.ml
ocamlc -g -c zstring.ml
ocamlc -g -c dictionary.ml
ocamlc -g -c object.ml
ocaml type.cmo utility.cmo immutable_bytes.cmo story.cmo zstring.cmo dictionary.cmo object.cmo flathead.ml
@del *.cmi
@del *.cmo
