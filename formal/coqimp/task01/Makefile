all: build_coq plot

plot: build_coq
	bash plot.sh

build_coq: CoqMakefile
	make -f CoqMakefile

Symlinks = float_text_io.v debug_printers.v

CoqMakefile: _CoqProject
	for f in $(Symlinks); do \
	  ln -s ../common-sim/$$f . ;\
	done; \
	coq_makefile -f _CoqProject -o CoqMakefile

clean:
	rm -f *.vo *.vos *.vok *.glob *~ .*.aux CoqMakefile CoqMakefile.conf .coqdeps.d $(Symlinks) .CoqMakefile.d


