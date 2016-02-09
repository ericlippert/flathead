#!/bin/sh
rm *.cmi 2> /dev/null
rm *.cmo 2> /dev/null
ocamlc -g -c type.ml
ocamlc -g -c utility.ml
ocamlc -g -c immutable_bytes.ml
ocaml type.cmo utility.cmo immutable_bytes.cmo flathead.ml
rm *.cmi 2> /dev/null
rm *.cmo 2> /dev/null
