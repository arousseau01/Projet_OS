CC 		= gcc
CFLAGS 	+= -Wall -g -Wextra
INCLUDE += -I./lib
LDLIB	+= -L./lib -lmpi_subset

LIB 	= ./lib
TEST 	= ./test

.PHONY: clean run_hello run_pi

LIST_TEST = $(TEST)/test_hello $(TEST)/test_pi

all: $(LIB)/libmpi_subset.a $(LIST_TEST)

$(LIB)/kahn.o: $(LIB)/kahn.c $(LIB)/kahn.h
	$(CC) $< $(CFLAGS) -c -o $@

$(LIB)/mpi_subset.o: $(LIB)/mpi_subset.c $(LIB)/mpi_subset.h
	$(CC) $< $(CFLAGS) -c -o $@

$(LIB)/libmpi_subset.a: $(LIB)/kahn.o $(LIB)/mpi_subset.o
	ar rcu $@ $+
	ranlib $@

$(TEST)/%: $(TEST)/%.o $(LIB)/libmpi_subset.a
	$(CC) $< $(CFLAGS) -o $@  $(LDLIB)

$(TEST)/%.o: $(TEST)/%.c 
	$(CC) $< $(CFLAGS) -c -o $@ $(INCLUDE)

run-hello:
	$(TEST)/test_hello -np 4

run-pi:
	$(TEST)/test_pi -np 4

clean: 
	rm $(TEST)/*.o $(LIB)/*.o $(LIST_TEST) $(LIB)/*.a