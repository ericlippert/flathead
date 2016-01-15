del flatheadprof.exe
ocamlcp -p a graphics.cma flathead.ml -o flatheadprof.exe
flatheadprof.exe
ocamlprof flathead.ml > flatheadprof.ml
del flathead.cmi
del flathead.cmo
del flatheadprof.exe
del ocamlprof.dump

