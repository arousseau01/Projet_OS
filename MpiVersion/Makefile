#OCAMLBUILD= ocamlbuild \
		-classic-display \
		-libs str,mpi \
		-tags debug,annot

OCAMLBUILD=ocamlbuild \
		-classic-display \
		-use-ocamlfind -pkgs mpi \
		-tags debug,annot

MAIN=testMPI
TARGET=native

all: $(MAIN)

$(MAIN): $(MAIN).$(TARGET)
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).$(TARGET):
	$(OCAMLBUILD) $(MAIN).$(TARGET)

run: $(MAIN)
	mpirun -np 4 ./$(MAIN)

clean:
	ocamlbuild -clean

realclean: clean
	rm -f *~ $(MAIN)

cleanall: realclean
