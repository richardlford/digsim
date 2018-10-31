PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
digsim.o: digsim.c $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
def_data.o: def_data.c $(DRV)global.h $(DRV)sysvars.h
diffeq.o: diffeq.c $(DRV)global.h $(DRV)sysvars.h
drvrdata.o: $(DRV)drvrdata.c $(DRV)global.h $(DRV)sysvars.h
drvrinit.o: drvrinit.c $(DRV)global.h $(DRV)sysvars.h $(DRV)setval.h $(DRV)print.h
init.o: init.c $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
setup.o: setup.c $(DRV)global.h $(DRV)sysvars.h $(DRV)setval.h
term.o: term.c $(DRV)global.h $(DRV)sysvars.h
prn_data.o: prn_data.c $(DRV)global.h $(DRV)sysvars.h $(DRV)print.h

advance.o: $(DRV)advance.c $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
defstate.o: $(DRV)defstate.c $(DRV)deriv.h
delim.o: $(DRV)delim.c
deriv.o: $(DRV)deriv.c $(DRV)deriv.h $(DRV)global.hs
global.o: $(DRV)global.c $(DRV)global.h $(DRV)digtypes.h
input.o: $(DRV)input.c $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)digtypes.h
parser.o: $(DRV)parser.c $(DRV)parser.h $(DRV)digtypes.h
print.o: $(DRV)print.c $(DRV)print.h $(DRV)global.h
prn_card.o: $(DRV)prn_card.c $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)print.h
setval.o: $(DRV)setval.c $(DRV)setval.h $(DRV)global.h
set_card.o: $(DRV)set_card.c $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)setval.h
upcase.o: $(DRV)upcase.c  $(DRV)digtypes.h
zerocomm.o: $(DRV)zerocomm.c $(DRV)global.h


OBJECTS = def_data.o diffeq.o digsim.o drvrinit.o init.o setup.o term.o
OBJECTS +=  $(DRV)advance.o $(DRV)defstate.o $(DRV)delim.o $(DRV)drvrdata.o $(DRV)input.o $(DRV)parser.o $(DRV)prn_card.o prn_data.o $(DRV)set_card.o $(DRV)upcase.o $(DRV)zerocomm.o $(DRV)global.o $(DRV)deriv.o $(DRV)setval.o $(DRV)print.o

.PHONY: clean $(INPUT)

CC = gcc
# CC = ccomp
CFLAGS = -g -Wall

$(PROGRAM): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

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
