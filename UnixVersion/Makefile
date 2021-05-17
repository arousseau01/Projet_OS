OCAMLBUILD= ocamlbuild \
		-no-links -classic-display \
		-libs str,unix \
		-tags debug,annot

MAIN = Version_Unix
CRIBLE = crible
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

clean:
	ocamlbuild -clean

realclean: clean
	rm -f *~ $(MAIN)
	rm -f *~ $(CRIBLE)

cleanall: realclean
