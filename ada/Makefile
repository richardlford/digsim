ADA = gnatmake -o digsim
ADAFLAGS = -I../driver -g -cargs -gnatq -gnatQ -bargs  -largs
all:
	cd Build01; gnatmake digsim -I../driver; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	cd Build02; gnatmake digsim -I../driver; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	cd Build03; gnatmake digsim -I../driver; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	#cd Build04; gnatmake digsim; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	#cd Build05; gnatmake digsim; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	#cd Build06; gnatmake digsim; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	#cd Build07; gnatmake digsim; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	#cd Build08; gnatmake digsim; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	#cd Build09; gnatmake digsim; ./digsim >./output; gnuplot ./digsim.plt; cd ..
	#cd Build10; gnatmake digsim; ./digsim >./output; gnuplot ./digsim.plt; cd ..
