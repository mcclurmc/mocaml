
FILTERS:=mock_filter.ml

# $(FILTERS:.ml=.cmo): ${@:.cmo=.ml}
# 	ocamlfind ocamlc -package camlp4.quotations.o -syntax camlp4o -c ${@:.cmo=.ml}

mock_foo.cmo: mock_foo.ml
	ocamlc.opt -c $<

mock_filter.cmo: mock_filter.ml
	ocamlc.opt -I +camlp4 -c -pp camlp4orf $<

print.cmo: print.ml
	ocamlc.opt -I +camlp4 -c -pp camlp4orf $<

.PHONY: foo_mock.ml
foo_mock.ml: foo.mli mock_filter.cmo
	camlp4o mock_filter.cmo foo.mli

.PHONY: printer
printer: print.cmo
	camlp4 -printer $<
