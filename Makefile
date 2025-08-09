default:
	@echo "Available targets:"
	@echo "  deps-linux"
	@echo "  deps-macos"
	@echo "  build (b)"
	@echo "  build-watch (w)"
	@echo "  test (t)"
	@echo "  clean"
	@echo "  format"

deps:
	opam switch create --no-install . 5.3.0 || true
	opam install -y --deps-only .
	opam install -y ocaml-lsp-server ocamlformat

deps-linux: deps
	sudo apt-get install -y libopus-dev

deps-macos: deps
	brew install opus
	ln -s /opt/homebrew/lib/libopus.dylib /usr/local/lib || true

build:
	rm -f yum
	dune build
	cp _build/default/bin/main.exe yum
	chmod +x yum

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
