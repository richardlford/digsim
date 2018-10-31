PROGRAM = digsim.exe
INPUT  = input.dat
OUTPUT = output.dat
PLOTS = output.pdf
PLOTFILE = digsim.plt
DRV = ../driver/
all: $(PROGRAM) $(OUTPUT) $(PLOTS) clean
OBJECTS = digsim.o
SOURCES = digsim.c
.PHONY: clean $(INPUT)

CC = gcc
#CC = ccomp
CFLAGS = -g -Wall # -fbounds-check

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
