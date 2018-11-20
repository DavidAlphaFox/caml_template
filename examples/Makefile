TOP_DIR=..
-include $(TOP_DIR)/Makefile.config
include $(TOP_DIR)/Makefile.rules

BUILD_EXAMPLES := hello filetest thread
ALL_EXAMPLES := $(BUILD_EXAMPLES) mod_caml fastcgi
INSTALL_DIR := $(PREFIX)/share/$(PKGBASE)

.PHONY: all
all:
	set -e; for dir in $(BUILD_EXAMPLES); do cd $$dir; $(MAKE) all; cd ..; done

.PHONY: clean
clean:
	for dir in $(ALL_EXAMPLES); do cd $$dir; $(MAKE) clean; cd ..; done
	rm -f *~

.PHONY: install
install:
	rm -rf $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)/examples
	cp $(TOP_DIR)/OCamlMakefile $(INSTALL_DIR)
	cp -r examples.rules $(ALL_EXAMPLES) $(INSTALL_DIR)/examples
	find $(INSTALL_DIR)/examples -type d -name CVS -prune -exec rm -rf \{} \;
	find $(INSTALL_DIR)/examples -type d -name ._d -prune -exec rm -rf \{} \;

.PHONY: uninstall
uninstall:
	rm -rf $(INSTALL_DIR)
