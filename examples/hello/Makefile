TOP_DIR := ../..
-include $(TOP_DIR)/Makefile.config
-include $(TOP_DIR)/Makefile.rules
include ../examples.rules

INCDIRS := $(CAMLTEMPLATE_DIR)
LIBS := unix str camltemplate

SOURCES := hello.ml
RESULT := hello

TRASH := *~

OCAMLMAKEFILE := $(TOP_DIR)/OCamlMakefile

.PHONY: all
all: byte-code

include $(TOP_DIR)/OCamlMakefile
