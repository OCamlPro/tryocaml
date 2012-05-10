
MLIS= int_misc.mli nat.mli big_int.mli arith_flags.mli ratio.mli num.mli arith_status.mli
MLS= int_misc.ml nat.ml big_int.ml arith_flags.ml ratio.ml num.ml arith_status.ml topnum.ml

all: ocaml-num.cma

ocaml-num.cma: $(MLIS) $(MLS)
	ocamlc -g -c $(MLIS)
	ocamlc -g -a -o ocaml-num.cma $(MLS)

OCamlNum.cmo: $(MLIS) $(MLS)
	ocamlc -g -c -for-pack OCamlNum $(MLIS)
	ocamlc -g -pack -o OCamlNum.cmo -for-pack OCamlNum $(MLS)

OCamlNum.cmx: $(MLIS) $(MLS)
	ocamlopt -g -c -for-pack OCamlNum $(MLIS)
	ocamlopt -g -for-pack OCamlNum $(MLS)
	ocamlopt -g -pack -o OCamlNum.cmx $(MLS:.ml=.cmx)

test: ocaml-num.cma test.ml
	ocamlc -g -o test ocaml-num.cma test.ml

clean:
	rm -f test ocaml-num.cma *.cm? *~ *.o

numtop: OCamlNum.cmo
	ocamlmktop -o numtop nums.cma OCamlNum.cmo

testdiv: OCamlNum.cmx tests/testdiv.ml
	(cd tests; ocamlopt -o testdiv nums.cmxa ../OCamlNum.cmx testdiv.ml)


