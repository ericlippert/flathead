@del *.cmi
@del *.cmo
ocamlc -g -c utility.ml
ocamlc -g -c deque.ml
ocamlc -g -c screen.ml
ocamlc -g -c immutable_bytes.ml
ocamlc -g -c memory.ml
ocamlc -g -c zstring.ml
ocamlc -g -c instruction.ml
ocamlc -g -c story.ml
ocamlc -g -c iff.ml
ocamlc -g -c quetzal.ml
ocamlc -g -c evaluation_stack.ml
ocamlc -g -c local_store.ml
ocamlc -g -c frame.ml
ocamlc -g -c frameset.ml
ocamlc -g -c randomness.ml
ocamlc -g -c transcript.ml
ocamlc -g -c interpreter.ml
ocamlc -g -c button.ml
ocamlc -g -c debugger.ml
ocaml utility.cmo deque.cmo screen.cmo immutable_bytes.cmo memory.cmo zstring.cmo instruction.cmo story.cmo iff.cmo quetzal.cmo evaluation_stack.cmo local_store.cmo frame.cmo frameset.cmo randomness.cmo transcript.cmo interpreter.cmo graphics.cma button.cmo debugger.cmo flathead.ml
@del *.cmi
@del *.cmo
