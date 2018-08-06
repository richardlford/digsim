PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
digsim.o: digsim.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)deriv.inc
def_data.o: def_data.f $(DRV)global.inc $(DRV)sysvars.inc
diffeq.o: diffeq.f $(DRV)global.inc $(DRV)sysvars.inc
drvrdata.o: drvrdata.f $(DRV)global.inc $(DRV)sysvars.inc
drvrinit.o: drvrinit.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)setval.inc $(DRV)print.inc
init.o: init.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)deriv.inc
setup.o: setup.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)setval.inc
term.o: term.f $(DRV)global.inc $(DRV)sysvars.inc
prn_data.o: prn_data.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)print.inc

advance.o: $(DRV)advance.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)deriv.inc
defstate.o: $(DRV)defstate.f $(DRV)deriv.inc
delim.o: $(DRV)delim.f
input.o: $(DRV)input.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)parser.inc
parser.o: $(DRV)parser.f $(DRV)parser.inc
prn_card.o: $(DRV)prn_card.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)parser.inc $(DRV)print.inc
set_card.o: $(DRV)set_card.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)parser.inc $(DRV)setval.inc
upcase.o: $(DRV)upcase.f
zerocomm.o: $(DRV)zerocomm.f $(DRV)global.inc

OBJECTS = def_data.o diffeq.o digsim.o drvrdata.o drvrinit.o init.o setup.o term.o
OBJECTS +=  $(DRV)advance.o $(DRV)defstate.o $(DRV)delim.o $(DRV)input.o $(DRV)parser.o $(DRV)prn_card.o prn_data.o $(DRV)set_card.o $(DRV)upcase.o $(DRV)zerocomm.o

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
