.PHONY: deps-linux deps-macos install i build b build-watch w test t clean format

default:
	@echo "Available targets:"
	@echo "  deps-linux"
	@echo "  deps-macos"
	@echo "  install (i)"
	@echo "  build (b)"
	@echo "  build-watch (w)"
	@echo "  test (t)"
	@echo "  clean"
	@echo "  format"

deps:
	opam switch create --no-install --repos ox=git+https://github.com/oxcaml/opam-repository.git,default . 5.2.0+ox || true
	opam install -y --deps-only .
	opam install -y ocaml-lsp-server ocamlformat

deps-linux: deps
	sudo apt-get install -y libopus-dev

deps-macos: deps
	brew install opus
	ln -s /opt/homebrew/lib/libopus.dylib /usr/local/lib || true

install:
	opam install -y --deps-only .

i: install

build:
	rm -f yum
	dune build --release
	cp _build/install/default/bin/yum yum
	chmod +wx yum

b: build

build-watch:
	dune build -w

w: build-watch

test:
	dune runtest || true
	dune promote

t: test

clean:
	dune clean

format:
	find bin -type f -name *.ml -o -name *.mli | xargs ocamlformat --inplace
	find lib -type f -name *.ml -o -name *.mli | xargs ocamlformat --inplace
