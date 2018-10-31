PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
airframe.o: airframe.c airframe.h $(DRV)global.h $(DRV)sysvars.h
def_data.o: def_data.c def_data.h $(DRV)global.h $(DRV)sysvars.h
diffeq.o: diffeq.c diffeq.h $(DRV)global.h $(DRV)sysvars.h
fcomp.o: fcomp.c fcomp.h $(DRV)global.h $(DRV)sysvars.h
init.o: init.c init.h $(DRV)sysvars.h $(DRV)deriv.h
kmtc.o: kmtc.c kmtc.h $(DRV)global.h $(DRV)sysvars.h
seeker.o: seeker.c seeker.h $(DRV)global.h $(DRV)sysvars.h
term.o: term.c term.h $(DRV)global.h $(DRV)sysvars.h
OBJECTS = airframe.o def_data.o diffeq.o discrete.o fcomp.o init.o kmtc.o term.o seeker.o
SOURCES = airframe.c def_data.c diffeq.c discrete.c fcomp.c init.c kmtc.c term.c seeker.c \
          airframe.h def_data.h diffeq.h discrete.h fcomp.h init.h kmtc.h term.h seeker.h

advance.o: $(DRV)advance.c $(DRV)advance.h $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
arctan.o: $(DRV)arctan.c $(DRV)arctan.h
defstate.o: $(DRV)defstate.c $(DRV)defstate.h $(DRV)deriv.h
delim.o: $(DRV)delim.c $(DRV)delim.h
deriv.o: $(DRV)deriv.c $(DRV)deriv.h
digsim.o: $(DRV)digsim.c $(DRV)digsim.h $(DRV)global.h $(DRV)sysvars.h $(DRV)deriv.h
drvrdata.o: $(DRV)drvrdata.c $(DRV)drvrdata.h $(DRV)global.h $(DRV)sysvars.h
drvrinit.o: $(DRV)drvrinit.c $(DRV)drvrinit.d $(DRV)global.h $(DRV)sysvars.h $(DRV)setval.h $(DRV)print.h $(DRV)event.h
event.o: $(DRV)event.c $(DRV)event.h
global.o: $(DRV)global.c $(DRV)global.h
input.o: $(DRV)input.c $(DRV)input.d $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h
limit.o: $(DRV)limit.c $(DRV)limit.h
parser.o: $(DRV)parser.c $(DRV)parser.h
print.o: $(DRV)print.c $(DRV)print.h
prn_card.o: $(DRV)prn_card.c $(DRV)prn_card.h $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)print.h
prn_data.o: $(DRV)prn_data.c $(DRV)prn_data.h $(DRV)global.h $(DRV)sysvars.h $(DRV)print.h $(DRV)event.prm
procevnt.o: $(DRV)procevnt.c $(DRV)procevnt.h $(DRV)global.h $(DRV)sysvars.h $(DRV)event.h $(DRV)event.prm
rmevent.o:  $(DRV)rmevent.c  $(DRV)rmevent.h  $(DRV)global.h $(DRV)sysvars.h $(DRV)event.h
schedule.o: $(DRV)schedule.c $(DRV)schedule.h $(DRV)global.h $(DRV)sysvars.h $(DRV)event.h
set_card.o: $(DRV)set_card.c $(DRV)set_card.h $(DRV)global.h $(DRV)sysvars.h $(DRV)parser.h $(DRV)setval.h
setup.o: $(DRV)setup.c $(DRV)setup.h $(DRV)global.h $(DRV)sysvars.h $(DRV)setval.h $(DRV)print.h $(DRV)event.h $(DRV)event.prm
setval.o: $(DRV)setval.c $(DRV)setval.h
upcase.o: $(DRV)upcase.c $(DRV)upcase.h
zerocomm.o: $(DRV)zerocomm.c $(DRV)zerocomm.h $(DRV)global.h
OBJECTS += $(DRV)advance.o  $(DRV)arctan.o $(DRV)defstate.o $(DRV)delim.o $(DRV)deriv.o $(DRV)digsim.o $(DRV)drvrdata.o $(DRV)drvrinit.o $(DRV)event.o $(DRV)global.o $(DRV)input.o $(DRV)limit.o $(DRV)parser.o $(DRV)print.o $(DRV)prn_card.o $(DRV)prn_data.o $(DRV)procevnt.o $(DRV)rmevent.o $(DRV)schedule.o $(DRV)set_card.o $(DRV)setup.o $(DRV)setval.o $(DRV)upcase.o $(DRV)zerocomm.o
.PHONY: clean $(INPUT)

.PHONY: clean $(INPUT)

CC = gcc -I. -I../driver
#CC = ccomp -I. -I../driver
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

