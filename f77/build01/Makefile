PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
OBJECTS = digsim.o
SOURCES = digsim.f
.PHONY: clean $(INPUT)

FC = gfortran
FFLAGS = -g # -Wall -fbounds-check

$(PROGRAM): $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $^

$(OUTPUT): $(PROGRAM)
	./$(PROGRAM) > $(OUTPUT)

$(PLOTS): input.dat $(PLOTFILE)
	gnuplot $(PLOTFILE)

clean:
	rm -f $(OBJECTS) $(PROGRAM)

%: %.o
	$(FC) $(FFLAGS) -c -o $@ $<

%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $<
