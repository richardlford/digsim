PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
advance.o: advance.f global.inc sysvars.inc deriv.inc
def_data.o: def_data.f global.inc sysvars.inc
diffeq.o: diffeq.f global.inc sysvars.inc
init.o: init.f global.inc sysvars.inc deriv.inc
term.o: term.f global.inc sysvars.inc
digsim.o: digsim.f
OBJECTS = advance.o def_data.o diffeq.o digsim.o init.o term.o
SOURCES = advance.f def_data.f deriv.inc diffeq.f digsim.f global.inc init.f Makefile sysvars.inc term.f
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
