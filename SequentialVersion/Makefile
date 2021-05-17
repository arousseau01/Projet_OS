OCAMLBUILD= ocamlbuild \
		-no-links -classic-display \
		-libs str,unix \
		-tags debug,annot

MAIN = version_sequentielle
CRIBLE = crible
STRASSEN = strassen
TARGET = native

all: $(MAIN)

$(MAIN): $(MAIN).$(TARGET)
	cp _build/$(MAIN).$(TARGET) $(MAIN)

$(MAIN).$(TARGET):
	$(OCAMLBUILD) $(MAIN).$(TARGET)

crible: $(CRIBLE)

$(CRIBLE): $(CRIBLE).$(TARGET)
	cp _build/$(CRIBLE).$(TARGET) $(CRIBLE)

$(CRIBLE).$(TARGET):
	$(OCAMLBUILD) $(CRIBLE).$(TARGET)

strassen: $(STRASSEN)

$(STRASSEN): $(STRASSEN).$(TARGET)
	cp _build/$(STRASSEN).$(TARGET) $(STRASSEN)

$(STRASSEN).$(TARGET):
	$(OCAMLBUILD) $(STRASSEN).$(TARGET)

clean:
	ocamlbuild -clean

realclean: clean
	rm -f *~ $(MAIN)
	rm -f *~ $(CRIBLE)
	rm -f *~ $(STRASSEN)

cleanall: realclean
