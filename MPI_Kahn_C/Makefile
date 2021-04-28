CC = gcc
CFLAGS += -Wall -g -Wextra

all: test

test.o: test.c kahn.h mpi_subset.h
	$(CC) $< $(CFLAGS) -c -o $@ 

kahn.o: kahn.c kahn.h
	$(CC) $< $(CFLAGS) -c -o $@

mpi_subset.o: mpi_subset.c mpi_subset.h
	$(CC) $< $(CFLAGS) -c -o $@

test: test.o kahn.o mpi_subset.o
	$(CC) -o $@ $^ $(CFLAGS)

clean: 
	rm test test.o kahn.o mpi_subset.o