
OCAMLC:=ocamlfind ocamlc
TOP_DIR := ..
-include $(TOP_DIR)/Makefile.config
include $(TOP_DIR)/Makefile.rules

MLY:=ctParser.mly
MLL:=ctLexer.mll

SOURCES :=\
	ctUtil.ml ctSourcePos.ml ctExceptions.ml ctTemplateModel.ml \
	ctScope.ml ctTemplateTypes.ml ctContext.ml \
	ctExpression.ml ctIdent.ml ctUnop.ml \
	ctBinop.ml ctLiteral.ml ctFunctionCall.ml ctHashLookup.ml \
	ctStatement.ml ctMacro.ml \
	ctParserAux.ml ctParser.mli ctParser.ml ctLexer.ml \
	ctWeb.ml ctTemplate.ml ctCache.mli ctCache.ml \
	camlTemplate.mli camlTemplate.ml

ALL_ML:=$(filter %.ml,$(SOURCES))

ALL_CMO:=$(foreach s,$(ALL_ML), $(basename $(notdir $(s))).cmo)
ALL_CMX:=$(foreach s,$(ALL_ML), $(basename $(notdir $(s))).cmx)

FLAGS:=
OPTS:=
LIB_POSTFIX:=

all:
	ocamllex $(MLL) && \
	ocamlyacc $(MLY) && \
	ocamlfind ocamlc $(OPTS) -linkpkg -c $(SOURCES) && \
	ocamlfind ocamlc -thread -c camlTemplate_mt.ml && \
	ocamlfind ocamlc -a -o camltemplate.cma $(ALL_CMO)

opt:
	ocamllex $(MLL) && \
	ocamlyacc $(MLY) && \
	ocamlfind ocamlopt $(OPTS) -linkpkg -c $(SOURCES) && \
	ocamlfind ocamlopt -thread -c camlTemplate_mt.ml && \
	ocamlfind ocamlopt -a -o camltemplate.cmxa $(ALL_CMX)

install:
	files=`$(COLLECT_FILES) META camlTemplate.mli camlTemplate.cmi camltemplate.cma camltemplate.cmxa camltemplate.a camlTemplate_mt.cmo camlTemplate_mt.cmx camlTemplate_mt.o` && \
	$(OCAMLFIND) install -destdir $(PREFIX) camltemplate $$files

uninstall:
	$(OCAMLFIND) remove -destdir $(PREFIX) camltemplate

clean:
	rm -rf *.o *.a *.cm? mt_cmis *.cm?a ctLexer.ml ctParser.ml ctParser.mli *~
	rm -rf ._bcdi ._ncdi ._d
