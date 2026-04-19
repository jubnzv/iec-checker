# Opam Mirrors Based on OCaml 5.5
FROM ocaml/opam:ubuntu-22.04-ocaml-5.5

WORKDIR /home/opam/src

COPY dune-project \
  iec_checker.opam \
  requirements.txt \
  requirements-dev.txt \
  /home/opam/src

# Switch to root to modify apt sources and install 
USER root

# install python3 and graphviz
RUN apt-get update && \
    apt-get install -y python3 python3-pip python3-venv graphviz libgraphviz-dev pkg-config && \
    rm -rf /var/lib/apt/lists/*

USER opam

# Install project dependencies (dune-project defines dependencies)
# Note: opam install . By default installs both dependencies and the project itself.
RUN opam install . --deps-only -y && \
    opam clean -a -c -s --logs && \
    pip install --no-cache-dir -r requirements-dev.txt && \
    pip install --no-cache-dir -r requirements.txt
