CC = gcc
CFLAGS += -Wall -g -Wextra

all: test_hello test_pi

test_hello.o: test_hello.c kahn.h mpi_subset.h
	$(CC) $< $(CFLAGS) -c -o $@ 

test_pi.o: test_pi.c kahn.h mpi_subset.h
	$(CC) $< $(CFLAGS) -c -o $@ 

kahn.o: kahn.c kahn.h
	$(CC) $< $(CFLAGS) -c -o $@

mpi_subset.o: mpi_subset.c mpi_subset.h
	$(CC) $< $(CFLAGS) -c -o $@

test_hello: test_hello.o kahn.o mpi_subset.o
	$(CC) -o $@ $^ $(CFLAGS)

test_pi: test_pi.o kahn.o mpi_subset.o
	$(CC) -o $@ $^ $(CFLAGS)

clean: 
	rm test_hello test_hello.o kahn.o mpi_subset.o test_pi test_pi.o