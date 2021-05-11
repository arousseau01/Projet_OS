CC = gcc
CFLAGS += -Wall -g -Wextra

all: test

test.o: test.c kahn.h
	$(CC) $< $(CFLAGS) -c -o $@ 

kahn.o: kahn.c kahn.h
	$(CC) $< $(CFLAGS) -c -o $@

test: test.o kahn.o
	$(CC) -o $@ $^ $(CFLAGS)

clean: 
	rm test test.o kahn.o