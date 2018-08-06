all: input.dat clean
	$(MAKE) clean
	gnatmake -I../driver -D build -g digsim
	./digsim
	gnuplot digsim.plt
	../../comparedat.py

.PHONY: clean input.dat

clean:
	rm -rf *~ digsim build b~* *~

