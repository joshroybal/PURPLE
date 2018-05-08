all: purple maketab

purple: purple.o
	gfortran -static -s -o purple purple.f95

maketab: maketab.o
	gfortran -s -o maketab maketab.f95

purple.o: purple.f95
	gfortran -O2 -c purple.f95

maketab.o: maketab.f95
	gfortran -O2 -c maketab.f95

clean:
	rm *.o && rm maketab && rm purple
