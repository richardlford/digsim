PROGRAM = digsim.exe
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt

.PHONY: clean

all: $(PROGRAM) $(OUTPUT) $(PLOTS)

$(PROGRAM): digsim.hs
	ghc --make -Wall -o $(PROGRAM) $^

$(OUTPUT): $(PROGRAM)
	./$(PROGRAM) > $(OUTPUT)

$(PLOTS): $(PLOTFILE) $(OUTPUT)
	gnuplot $<

clean:
	rm -f $(PROGRAM) $(OUTPUT) $(PLOTS) digsim.hi digsim.o
