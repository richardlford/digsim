PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
digsim.o: digsim.c $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
def_data.o: def_data.c def_data.h $(DRV)global.h $(DRV)sysvars.h
diffeq.o: diffeq.c diffeq.h $(DRV)global.h $(DRV)sysvars.h
init.o: init.c $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
setup.o: setup.c $(DRV)global.h $(DRV)sysvars.h $(DRV)setval.h
term.o: term.c term.h $(DRV)global.h $(DRV)sysvars.h

advance.o: $(DRV)advance.c $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
defstate.o: $(DRV)defstate.c $(DRV)deriv.h
delim.o: $(DRV)delim.c
deriv.o: $(DRV)deriv.c $(DRV)deriv.h $(DRV)global.hs
drvrdata.o: $(DRV)drvrdata.c $(DRV)drvrdata.h $(DRV)global.h $(DRV)sysvars.h
drvrinit.o: $(DRV)drvrinit.c $(DRV)drvrinit.h $(DRV)global.h $(DRV)sysvars.h $(DRV)setval.h $(DRV)print.h
global.o: $(DRV)global.c $(DRV)global.h $(DRV)digtypes.h
event.o: $(DRV)event.c $(DRV)event.h
input.o: $(DRV)input.c $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)digtypes.h
parser.o: $(DRV)parser.c $(DRV)parser.h $(DRV)digtypes.h
print.o: $(DRV)print.c $(DRV)print.h $(DRV)global.h
prn_card.o: $(DRV)prn_card.c $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)print.h
prn_data.o: $(DRV)prn_data.c $(DRV)prn_data.h $(DRV)global.h $(DRV)sysvars.h $(DRV)print.h
procevnt.o: $(DRV)procevnt.c $(DRV)procevnt.h $(DRV)event.h $(DRV)global.h $(DRV)prn_data.h $(DRV)rmevent.h $(DRV)sysvars.h discrete.h
set_card.o: $(DRV)set_card.c $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)setval.h
setval.o: $(DRV)setval.c $(DRV)setval.h $(DRV)global.h
schedule.o: $(DRV)schedule.c $(DRV)schedule.h $(DRV)global.h $(DRV)sysvars.h $(DRV)event.h

upcase.o: $(DRV)upcase.c  $(DRV)digtypes.h
zerocomm.o: $(DRV)zerocomm.c $(DRV)global.h

OBJECTS = def_data.o diffeq.o discrete.o init.o term.o
OBJECTS += $(DRV)advance.o $(DRV)defstate.o $(DRV)delim.o $(DRV)deriv.o $(DRV)digsim.o $(DRV)drvrdata.o $(DRV)drvrinit.o $(DRV)event.o $(DRV)global.o $(DRV)input.o $(DRV)parser.o $(DRV)print.o $(DRV)prn_card.o $(DRV)prn_data.o $(DRV)procevnt.o $(DRV)rmevent.o $(DRV)schedule.o $(DRV)set_card.o $(DRV)setup.o $(DRV)setval.o $(DRV)upcase.o $(DRV)zerocomm.o

.PHONY: clean $(INPUT)

#CC = gcc -I. -I../driver
CC = ccomp -I. -I../driver
CFLAGS = -g -Wall
CLIBS = -lm

$(PROGRAM): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ $(CLIBS)

$(OUTPUT): $(PROGRAM)
	./$(PROGRAM) > $(OUTPUT)

$(PLOTS): input.dat $(PLOTFILE)
	gnuplot $(PLOTFILE)

clean:
	rm -f $(OBJECTS) $(PROGRAM) *~

%: %.o
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

