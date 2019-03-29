all: CoqMakefile
	make -f CoqMakefile

CoqMakefile: _CoqProject
	coq_makefile -f _CoqProject -o CoqMakefile

clean:
	rm -f *.vo *.glob *~ .*.aux CoqMakefile CoqMakefile.conf .coqdeps.d

