PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
airframe.o: airframe.f $(DRV)global.inc $(DRV)sysvars.inc
def_data.o: def_data.f $(DRV)global.inc $(DRV)sysvars.inc
diffeq.o: diffeq.f $(DRV)global.inc $(DRV)sysvars.inc
fcomp.o: fcomp.f $(DRV)global.inc $(DRV)sysvars.inc
init.o: init.f $(DRV)sysvars.inc $(DRV)deriv.inc
kmtc.o: kmtc.f $(DRV)global.inc $(DRV)sysvars.inc
seeker.o: seeker.f $(DRV)global.inc $(DRV)sysvars.inc
term.o: term.f $(DRV)global.inc $(DRV)sysvars.inc
OBJECTS = airframe.o def_data.o diffeq.o discrete.o fcomp.o init.o kmtc.o term.o seeker.o
SOURCES = airframe.f def_data.f diffeq.f discrete.f fcomp.f init.f kmtc.f term.f seeker.f
advance.o: $(DRV)advance.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)deriv.inc
arctan.o: $(DRV)arctan.f
defstate.o: $(DRV)defstate.f $(DRV)deriv.inc
delim.o: $(DRV)delim.f
digsim.o: $(DRV)digsim.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)deriv.inc
drvrdata.o: $(DRV)drvrdata.f $(DRV)global.inc $(DRV)sysvars.inc
drvrinit.o: $(DRV)drvrinit.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)setval.inc $(DRV)print.inc $(DRV)event.inc
input.o: $(DRV)input.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)parser.inc
limit.o: $(DRV)limit.f
parser.o: $(DRV)parser.f $(DRV)parser.inc
prn_card.o: $(DRV)prn_card.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)parser.inc $(DRV)print.inc
prn_data.o: $(DRV)prn_data.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)print.inc $(DRV)event.prm
procevnt.o: $(DRV)procevnt.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)event.inc $(DRV)event.prm
rmevent.o: $(DRV)rmevent.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)event.inc
schedule.o: $(DRV)schedule.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)event.inc
set_card.o: $(DRV)set_card.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)parser.inc $(DRV)setval.inc
setup.o: $(DRV)setup.f $(DRV)global.inc $(DRV)sysvars.inc $(DRV)setval.inc $(DRV)print.inc $(DRV)event.inc $(DRV)event.prm
upcase.o: $(DRV)upcase.f
zerocomm.o: $(DRV)zerocomm.f $(DRV)global.inc
OBJECTS += $(DRV)advance.o  $(DRV)arctan.o $(DRV)defstate.o $(DRV)delim.o $(DRV)digsim.o $(DRV)drvrdata.o $(DRV)drvrinit.o $(DRV)input.o $(DRV)limit.o $(DRV)parser.o $(DRV)prn_card.o $(DRV)prn_data.o $(DRV)procevnt.o $(DRV)rmevent.o $(DRV)schedule.o $(DRV)set_card.o $(DRV)setup.o $(DRV)upcase.o $(DRV)zerocomm.o
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
