tb2hr2.x: tb2hr.f90 utils.o
	gfortran -o $@ $^  

utils.o: utils.f90
	gfortran -c utils.f90

clean:
	rm *.o *.mod *.x
