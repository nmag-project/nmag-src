#!/bin/sh

rm  *.mli *.cmo diffop
ocamllex diffop_lexer.mll
ocamlyacc diffop_parser.mly
cat HACK_TYPES diffop_parser.mli >dp.mli
mv dp.mli diffop_parser.mli
ocamlc -c diffop_parser.mli
ocamlc -c diffop_lexer.ml
ocamlc -c diffop_parser.ml
ocamlc -c diffop.ml
ocamlc -o diffop diffop_lexer.cmo diffop_parser.cmo diffop.cmo