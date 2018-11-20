TOP_DIR := ../..
-include $(TOP_DIR)/Makefile.rules
include ../examples.rules

# Change this to the directory where you've compiled Christophe
# Troestler's CamlGI package.
CAMLGI_DIR = ../../../CamlGI-0.5

# Change this to the directory where you install CGI programs to be run
# by FastCGI.
FCGI_BINDIR := /var/www/fcgi-bin

TEMPLATEDIR := $(FCGI_BINDIR)/templates

INCDIRS := -I $(CAMLGI_DIR) -I $(CAMLTEMPLATE_DIR)

BYTE_LIBS := unix.cma threads.cma str.cma camlGI.cma camltemplate.cma
BYTE_LIBS+=camlTemplate_mt.cmo

NATIVE_LIBS := unix.cmxa threads.cmxa str.cmxa camlGI.cmxa camltemplate.cmxa
NATIVE_LIBS+=camlTemplate_mt.cmx

SOURCES := fcgihello.ml
RESULT := fcgihello

.PHONY: all opt clean install
all: 
	ocamlc -thread $(INCDIRS) -o $(RESULT) $(BYTE_LIBS) $(SOURCES)

opt:
	ocamlopt -thread $(INCDIRS) -o $(RESULT) $(NATIVE_LIBS) $(SOURCES)

clean:
	rm -rf $(RESULT) *.cm? *.o *~ ._d

install:
	mkdir -p $(FCGI_BINDIR)
	cp fcgihello $(FCGI_BINDIR)
	mkdir -p $(TEMPLATEDIR)
	cp fcgihello.tmpl $(TEMPLATEDIR)
