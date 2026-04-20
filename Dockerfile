FROM ocaml/opam:ubuntu-22.04-ocaml-5.2

USER root

RUN apt-get update && \
    apt-get install -y python3 python3-pip graphviz libgraphviz-dev pkg-config && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /home/opam/src

COPY requirements*.txt ./
RUN pip3 install --no-cache-dir -r requirements-dev.txt -r requirements.txt

COPY --chown=opam iec_checker.opam dune-project ./

USER opam

RUN opam install . --deps-only -y && \
    opam clean -a -c -s --logs
