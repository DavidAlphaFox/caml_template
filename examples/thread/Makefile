TOP_DIR := ../..
-include $(TOP_DIR)/Makefile.rules
include ../examples.rules

INCDIRS := -I $(shell ocamlfind query str) -I $(CAMLTEMPLATE_DIR)
BYTE_LIBS := unix.cma str.cma threads.cma camltemplate.cma camlTemplate_mt.cmo
NATIVE_LIBS := unix.cmxa str.cmxa threads.cmxa camltemplate.cmxa camlTemplate_mt.cmx

SOURCES := thread_test.ml
RESULT := thread_test

.PHONY: all opt clean
all: 
	ocamlc -thread $(INCDIRS) -o $(RESULT) $(BYTE_LIBS) $(SOURCES)

opt:
	ocamlopt -thread $(INCDIRS) -o $(RESULT) $(NATIVE_LIBS) $(SOURCES)

clean:
	rm -f $(RESULT) *.cm? *.o *~
