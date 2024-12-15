##################################################################################
#                                    openmx2tbx                                  #
#                                    ==========                                  #
#                                                                                #
#            N. N. Batista                                                       #
#            nnardoto@gmail.com                                                  #
#            23/Jul./2024                                                        #
#                                                                                #
#            Adaptation of Makefile from openmx@3.9 code                         #
#            https://www.openmx-square.org/                                      #
#                                                                                #
##################################################################################
CC = gcc -std=c99 -fcommon

openmx2tbx.x: openmx2tbx.o read_scfout.o indexing.o
	$(CC) read_scfout.o indexing.o openmx2tbx.o -o openmx2tbx.x -lm

openmx2tbx.o: openmx2tbx.c indexing.o
	$(CC) -c openmx2tbx.c -o openmx2tbx.o -lm

read_scfout.o: read_scfout.c read_scfout.h 
	$(CC) -c read_scfout.c -o read_scfout.o

indexing.o: indexing.h indexing.c
	$(CC) -c indexing.c -o indexing.o -lm

clean:
	rm openmx2tbx.x *.o
