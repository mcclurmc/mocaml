
FILTERS:=mock_filter.ml

# $(FILTERS:.ml=.cmo): ${@:.cmo=.ml}
# 	ocamlfind ocamlc -package camlp4.quotations.o -syntax camlp4o -c ${@:.cmo=.ml}

default: test_mockfoo

# mock_foo.cmo: mock_foo.ml
# 	ocamlc.opt -c $<

mock_filter.cmo: mock_filter.ml
	ocamlc.opt -I +camlp4 -c -pp camlp4orf $<

print.cmo: print.ml
	ocamlc.opt -I +camlp4 -c -pp camlp4orf $<

mock_foo.cmo: foo.mli mock_filter.cmo
	camlp4o mock_filter.cmo foo.mli
	cat mock_foo.ml
	ocamlc -c mock_foo.ml

.PHONY: printer
printer: print.cmo
	camlp4 -printer $<

foo.cmi: foo.mli
	ocamlc -c foo.mli

foo.cmo: foo.ml foo.cmi
	ocamlc -c foo.ml

test_mockfoo: foo.cmo mock_foo.cmo test_mockfoo.ml
	ocamlc -o $@ $^
