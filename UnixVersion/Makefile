OCAMLC = ocamlc

OCAMLFLAGS += unix.cma

MAIN=testUnix

all: $(MAIN)

$(MAIN): $(MAIN).ml

	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^
	

realclean: clean
	rm -f *~ $(MAIN)

cleanall: realclean
