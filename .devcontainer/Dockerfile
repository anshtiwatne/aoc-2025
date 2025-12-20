FROM ocaml/opam:latest

ENV LC_ALL=C.UTF-8

RUN opam install -y dune ocaml-lsp-server ocamlformat base stdio
RUN echo 'eval $(opam env)' >> ~/.bashrc
