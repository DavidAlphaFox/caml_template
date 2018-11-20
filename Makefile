DIRS := src examples doc
VERSION := $(shell grep version src/META | sed -e 's/version="//' -e 's/".*//')
DIST_DIR := camltemplate-$(VERSION)

# PKGBASE is provided by GODIVA.
ifeq ($(PKGBASE),)
PKGBASE := camltemplate
endif
export PKGBASE

# Builds bytecode.
.PHONY: all
all: Makefile.config
	cd src; $(MAKE) all
	cd examples; $(MAKE) all

# Builds native code.
.PHONY: opt
opt: Makefile.config
	cd src; $(MAKE) opt

# Builds HTML documentation.
.PHONY: htdoc
htdoc: Makefile.config
	cd doc; make htdoc

# Builds documentation in all formats.
.PHONY: pdfdoc
pdfdoc: Makefile.config
	cd doc; make pdfdoc

# Runs a regression test.
.PHONY: test
test:
	test/test.sh

# Installs the library.
.PHONY: install
install: Makefile.config
	cd src; $(MAKE) install
	cd doc; $(MAKE) install
	cd examples; $(MAKE) install

# Uninstalls the library.
.PHONY: uninstall
uninstall: Makefile.config
	cd src; $(MAKE) uninstall
	cd doc; $(MAKE) uninstall
	cd examples; $(MAKE) uninstall

# Creates a distribution tarball.
.PHONY: dist
dist: distclean all htdoc pdfdoc
	set -e; \
	./configure; \
	rm -rf $(DIST_DIR)*; \
	files=`ls`; \
	mkdir $(DIST_DIR); \
	cp -r $$files $(DIST_DIR); \
	find $(DIST_DIR) -type d -name CVS -prune -exec rm -rf \{} \;
	cd $(DIST_DIR); \
	for dir in $(DIRS); do \
		if [ $$dir != "doc" ]; then \
			cd $$dir; $(MAKE) clean; cd ..; \
		fi; \
	done; \
	cd doc; $(MAKE) distclean; cd ../..; \
	rm -f $(DIST_DIR)/Makefile.config; \
	tar zcf $(DIST_DIR).tar.gz $(DIST_DIR); \
	rm -rf $(DIST_DIR)

# Copies the distribution and docs into the SauceCode web site.
.PHONY: webinstall
webinstall: $(DIST_DIR).tar.gz
	cp -r doc/html/api doc/html/manual ../../saucecode-site/saucecode.org/htdocs/camltemplate
	mkdir -p ../../saucecode-site/saucecode.org/htdocs/camltemplate/releases
	cp $(DIST_DIR).tar.gz ../../saucecode-site/saucecode.org/htdocs/camltemplate/releases

# Deletes generated files.
.PHONY: clean
clean:
	for dir in $(DIRS); do cd $$dir; $(MAKE) clean; cd ..; done
	rm -rf *~ camltemplate-*

.PHONY: distclean
distclean: clean
	rm -f Makefile.config
	./configure
