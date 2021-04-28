OCAMLBUILD= ocamlbuild \
		-no-links -classic-display \
		-libs str,unix \
		-tags debug,annot

MAIN=test_mpi
TARGET=native

all: $(MAIN)

$(MAIN): $(MAIN).$(TARGET)
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).$(TARGET):
	$(OCAMLBUILD) $(MAIN).$(TARGET)


clean:
	ocamlbuild -clean

realclean: clean
	rm -f *~ $(MAIN)

cleanall: realclean
