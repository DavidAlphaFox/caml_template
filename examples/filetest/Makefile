TOP_DIR := ../..
-include $(TOP_DIR)/Makefile.config
-include $(TOP_DIR)/Makefile.rules
include ../examples.rules

INCDIRS := $(shell ocamlfind query ) $(CAMLTEMPLATE_DIR)
LIBS := unix str camltemplate

SOURCES := filetest.ml
RESULT := filetest

TRASH := *~ coleridge.html

OCAMLMAKEFILE := $(TOP_DIR)/OCamlMakefile

.PHONY: all
all: byte-code

include $(TOP_DIR)/OCamlMakefile
