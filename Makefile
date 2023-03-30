.PHONY: build test clean

default: build

build:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

test: build
	@/bin/bash -c "source venv/bin/activate; \
				   pushd test >/dev/null; \
				   pytest; \
				   popd >/dev/null; "

clean:
	dune clean
	git clean -dfXq --exclude=\!venv/**
