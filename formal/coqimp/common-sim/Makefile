all: build_coq_and_extract extraction_ml/digsim_driver.byte extraction_ml/my_printers.cma

extraction_ml/digsim_driver.byte: build_coq_and_extract
	(cd extraction_ml; ocamlbuild -r -tag 'debug' digsim_driver.byte)

extraction_ml/my_printers.cma: build_coq_and_extract
	(cd extraction_ml; ocamlbuild -r -tag 'debug' my_printers.cma)

build_coq_and_extract: CoqMakefile extraction_ml
	make -f CoqMakefile

Symlinks = driver1.v driver2.v driver3.v extractOcaml.v digsim_driver.ml my_printers.ml .ocamldebug
CoqMakefile: _CoqProject
	for f in $(Symlinks); do \
	  ln -s ../common-sim/$$f . ;\
	done; \
	coq_makefile -f _CoqProject -o CoqMakefile

extraction_ml:
	mkdir extraction_ml
	(cd extraction_ml; ln -s ../*.ml .; ln -s ../.camldebug .)

clean:
	rm -f *.vo *.glob *~ .*.aux CoqMakefile CoqMakefile.conf .coqdeps.d $(Symlinks)
	rm -rf extraction*
