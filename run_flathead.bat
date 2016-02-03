@del *.cmi
@del *.cmo
ocamlc -g -c type.ml
ocamlc -g -c utility.ml
ocamlc -g -c immutable_bytes.ml
ocamlc -g -c story.ml
ocamlc -g -c routine.ml
ocamlc -g -c zstring.ml
ocamlc -g -c dictionary.ml
ocamlc -g -c object.ml
ocamlc -g -c instruction.ml
ocamlc -g -c reachability.ml
ocamlc -g -c local_store.ml
ocaml type.cmo utility.cmo immutable_bytes.cmo story.cmo routine.cmo zstring.cmo dictionary.cmo object.cmo instruction.cmo reachability.cmo local_store.cmo flathead.ml
@del *.cmi
@del *.cmo
