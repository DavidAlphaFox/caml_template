TOP_DIR := ../..
-include $(TOP_DIR)/Makefile.rules
include ../examples.rules

# Change this to the directory where you install .cmo files to be run
# by mod_caml.
MODCAML_BINDIR := /var/www/caml-bin

# Change this to the directory where you want to install templates.
TEMPLATEDIR := /var/www/caml-templates

# Where to find the 'apache' package installed by mod_caml.
OCAMLLIBDIR := $(shell ocamlc -where)

INCDIRS := $(OCAMLLIBDIR)/apache $(CAMLTEMPLATE_DIR)
LIBS := unix str cgi camltemplate

SOURCES := cthello.ml

TRASH := *~ cthello.ml

OCAMLMAKEFILE := $(TOP_DIR)/OCamlMakefile

.PHONY: all
all: cthello.ml byte-code-nolink

cthello.ml: cthello.ml.in
	sed -e 's,@TEMPLATEDIR@,$(TEMPLATEDIR),g' \
	< $< > $@

.PHONY: install
install:
	mkdir -p $(MODCAML_BINDIR)
	cp cthello.cmo $(MODCAML_BINDIR)
	mkdir -p $(TEMPLATEDIR)
	cp cthello.tmpl $(TEMPLATEDIR)

include $(TOP_DIR)/OCamlMakefile
