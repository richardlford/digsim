all: build plot

plot: build
	bash plot.sh

build: build_coq_and_extract extraction_ml/digsim_driver.byte extraction_ml/my_printers.cma

extraction_ml/digsim_driver.byte: build_coq_and_extract
	(cd extraction_ml; export LD_LIBRARY_PATH=/home/fordrl/e/coq/kernel/byterun;ocamlbuild -r -tag 'debug' -lib /home/fordrl/e/coq/kernel/uint63.cmo -lib /home/fordrl/e/coq/kernel/float64.cmo digsim_driver.byte)

extraction_ml/my_printers.cma: build_coq_and_extract
	(cd extraction_ml; export LD_LIBRARY_PATH=/home/fordrl/e/coq/kernel/byterun;ocamlbuild -r -tag 'debug' -lib /home/fordrl/e/coq/kernel/uint63.cmo -lib /home/fordrl/e/coq/kernel/float64.cmo my_printers.cma)

build_coq_and_extract: CoqMakefile extraction_ml
	make -f CoqMakefile

Symlinks = build.sh coq_trig.ml debug_printers.v digsim_driver.ml \
           driver_requires.v driver_run.v driver_state.v \
           extract_haskell.v extract_ocaml_trig.v extract_ocaml.v extr_ocaml_int64_conv.v \
           float_text_io.v monad.v my_printers.ml plot.sh trig.v \
           float64.ml float64.mli uint63.mli uint63.ml

CoqMakefile: _CoqProject
	for f in $(Symlinks); do \
	  ln -s ../common-sim/$$f . ;\
	done; \
	coq_makefile -f _CoqProject -o CoqMakefile

_CoqProject:
	ln -s ../common-sim/_CoqProject .
extraction_ml:
	mkdir extraction_ml
	(cd extraction_ml; ln -s ../*.ml* .; ln -s ../.ocamldebug .ocamldebug)

clean:
	rm -f *.vo *.vos *.vok *.glob *~ .*.aux CoqMakefile CoqMakefile.conf .coqdeps.d $(Symlinks) _CoqProject .CoqMakefile.d
	rm -rf extraction_*

