all: purple formdata

purple: purple.o
	gfortran -s -o purple purple.o

formdata: formdata.o
	gfortran -s -o formdata formdata.o

purple.o: purple.f95
	gfortran -O2 -c purple.f95

formdata.o: formdata.f95
	gfortran -O2 -c formdata.f95

clean: 
	rm *.o && rm formdata && rm purple
