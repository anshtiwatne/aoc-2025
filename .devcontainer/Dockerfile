FROM ocaml/opam:latest

RUN opam install -y dune ocaml-lsp-server ocamlformat base stdio
RUN echo 'eval $(opam env)' >> ~/.bashrc
