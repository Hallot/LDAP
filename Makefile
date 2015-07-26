NAME:=HALLOT.PIERRE
DOC:=$(NAME)_rapport.pdf
SOURCES:=$(wildcard *.ml)
TARGETS:=ldap.ml
STATINT:=$(wildcard *.mli)
BIN_BYT:=$(patsubst %.ml, %.byte, $(TARGETS))
BIN_NAT:=$(patsubst %.ml, %.native, $(TARGETS))
#INTF :=$(patsubst %.ml, %.inferred.mli, $(SOURCES))

OCAMLBUILD := ocamlbuild -classic-display

all: native

native:
	$(OCAMLBUILD) $(BIN_NAT)

byte:
	$(OCAMLBUILD) $(BIN_BYT)

intf:
	$(OCAMLBUILD) $(INTF)

doc:
	$(OCAMLBUILD) ldap.docdir/index.html

clean:
	ocamlbuild -classic-display -clean

project:
	tar cvzf IIES1-IPF-pierre.hallo.tgz Makefile $(STATINT) $(SOURCES) $(DOC) #$(INTF)
