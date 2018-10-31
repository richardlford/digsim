PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
advance.o: advance.c advance.h global.h sysvars.h deriv.h
def_data.o: def_data.c def_data.h global.h sysvars.h
diffeq.o: diffeq.c diffeq.h global.h sysvars.h
init.o: init.c init.h global.h sysvars.h deriv.h
term.o: term.c term.h global.h sysvars.h
digsim.o: digsim.c advance.h def_data.h diffeq.h init.h term.h
deriv.o: deriv.c deriv.h
global.o: global.c global.h
SOURCES = advance.c def_data.c deriv.c diffeq.c global.c init.c sysvars.c term.c digsim.c Makefile
HEADERS = advance.h def_data.h deriv.h diffeq.h global.h init.h sysvars.h term.h
OBJECTS = advance.o def_data.o diffeq.o digsim.o init.o term.o global.o deriv.o
.PHONY: clean $(INPUT)

CC = gcc
#CC = ccomp
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
