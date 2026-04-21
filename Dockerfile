FROM ocaml/opam:ubuntu-22.04-ocaml-5.2 AS build-env

USER root

RUN apt-get update && \
    apt-get install -y python3 python3-pip graphviz libgraphviz-dev pkg-config && \
    rm -rf /var/lib/apt/lists/*

COPY requirements*.txt /tmp/
RUN pip3 install --no-cache-dir -r /tmp/requirements-dev.txt -r /tmp/requirements.txt

USER opam
WORKDIR /home/opam/src

COPY --chown=opam iec_checker.opam dune-project ./

RUN opam install . --deps-only -y && \
    opam clean -a -c -s --logs

COPY --chown=opam . .

RUN eval $(opam env) && make build

FROM ubuntu:22.04

COPY --from=build-env /home/opam/src/_build/install/default/bin/iec_checker /usr/local/bin/iec_checker

ENTRYPOINT ["iec_checker"]
