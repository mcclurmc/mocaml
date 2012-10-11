
FILTERS:=mock_filter.ml

# $(FILTERS:.ml=.cmo): ${@:.cmo=.ml}
# 	ocamlfind ocamlc -package camlp4.quotations.o -syntax camlp4o -c ${@:.cmo=.ml}

mock_filter.cmo: mock_filter.ml
	ocamlc.opt -I +camlp4 -c -pp camlp4orf $<

.PHONY: foo_mock.ml mock_filter.cmo
foo_mock.ml: foo.mli
	camlp4o mock_filter.cmo foo.mli