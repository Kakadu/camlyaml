.PHONY: all lib test clean

all:
	ocamlbuild -use-ocamlfind yaml.cma yaml.cmxa test.native

test: all
	./test.native

clean:
	rm -fr _build test*.native test*.byte

install:
	ocamlfind install yaml META _build/yaml.cma _build/yaml.cmxa

uninstall:
	ocamlfind remove yaml
