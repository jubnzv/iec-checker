.PHONY: build test doc clean

default: build

build:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

test: build
	@/bin/bash -c "source venv/bin/activate; \
				   pushd test >/dev/null; \
				   pytest; \
				   popd >/dev/null; "

doc:
	dune build @doc
	@echo "Docs: _build/default/_doc/_html/index.html"

clean:
	dune clean
	git clean -dfXq --exclude=\!venv/**
