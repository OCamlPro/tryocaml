
MLIS= int_misc.mli nat.mli big_int.mli arith_flags.mli ratio.mli num.mli arith_status.mli
MLS= int_misc.ml nat.ml big_int.ml arith_flags.ml ratio.ml num.ml arith_status.ml big.ml

all: ocaml-num.cma

ocaml-num.cma: $(MLIS) $(MLS)
	ocamlc -g -c $(MLIS)
	ocamlc -g -a -o ocaml-num.cma $(MLS)


test: ocaml-num.cma test.ml
	ocamlc -g -o test ocaml-num.cma test.ml

clean:
	rm -f test ocaml-num.cma *.cm? *~ *.o
