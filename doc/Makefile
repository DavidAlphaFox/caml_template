TOP_DIR=..
-include $(TOP_DIR)/Makefile.config
include $(TOP_DIR)/Makefile.rules

DOC_MLI := ../src/camlTemplate.mli
INSTALL_DIR := $(PREFIX)/doc/$(PKGBASE)

.PHONY: htdoc
htdoc: html/api/index.html html/manual/index.html

html/api/index.html: $(TOP_DIR)/src/camlTemplate.mli
	mkdir -p html
	rm -rf html/api
	mkdir -p html/api
	ocamldoc -I ../src -html -d html/api -t "CamlTemplate API Documentation" $(DOC_MLI)

html/manual/index.html: manual/manual.tex
	mkdir -p html
	rm -rf html/manual
	mkdir -p html/manual
	cd manual; \
	hevea -fix -O -o manual.html manual.tex; \
	hacha manual.html; \
	cp *.html *.css *.gif ../html/manual

.PHONY: pdfdoc
pdfdoc: pdf/manual.pdf

pdf/manual.pdf: manual/manual.tex
	set -e; rm -rf pdf; mkdir pdf; \
	cd manual; \
	latex manual.tex; \
	latex manual.tex; \
	dvips -o manual.ps manual.dvi; \
	ps2pdf manual.ps ../pdf/manual.pdf

.PHONY: install
install:
	rm -rf $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)
	mkdir -p html pdf
	cp -r html pdf $(INSTALL_DIR)

.PHONY: uninstall
uninstall:
	rm -rf $(INSTALL_DIR)

.PHONY: clean
clean:
	rm -rf html pdf *~
	cd manual; rm -f *~ *.html *.css *.gif *.log *.out *.aux *.dvi *.ps \
	   *.htoc *.image.tex *.haux *.pdf .log

.PHONY: distclean
distclean:
	cd manual; rm -f *.html *.css *.gif *.htoc *.image.tex *.haux *.aux \
		*.log *.dvi *.ps
