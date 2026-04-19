.PHONY: build test doc spell clean

default: build

build:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

test: build
	@/bin/bash -c "source venv/bin/activate; \
				   pushd test >/dev/null; \
				   python3 -m pytest; \
				   popd >/dev/null; "

doc:
	dune build @doc
	@echo "Docs: _build/default/_doc/_html/index.html"

spell:
	codespell

clean:
	dune clean
	git clean -dfXq --exclude=\!venv/**
