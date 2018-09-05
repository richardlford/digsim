PROGRAM = dist/build/DigSim/DigSim
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt

.PHONY: clean

all: $(PROGRAM) $(OUTPUT) $(PLOTS)

$(PROGRAM): dist DefData.hs DigSim.hs Main.hs
	runghc Setup.hs build

$(OUTPUT): $(PROGRAM)
	./$(PROGRAM) > $(OUTPUT)

$(PLOTS): $(PLOTFILE) $(OUTPUT)
	gnuplot $<

dist: Setup.hs
	runghc Setup.hs configure

clean: Setup.hs
	rm -f $(OUTPUT) $(PLOTS)
	runghc Setup.hs clean
