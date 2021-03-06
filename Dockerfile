FROM ocaml/opam2:alpine AS build

RUN mkdir -p /home/opam/app
WORKDIR /home/opam/app

RUN sudo apk add --update --no-cache m4 && \
  opam switch 4.09 && \
  eval $(opam env) && \
  opam update

COPY --chown=opam:nogroup echoes.opam .
RUN eval $(opam env) && opam install --deps-only .

COPY --chown=opam:nogroup . .

ARG VERSION=local
ARG SHA1=local
ARG BUILD=local
RUN sed -e "s/%%VERSION%%/${VERSION}/g" \
        -e "s/%%SHA1%%/${SHA1}/g" \
        -e "s/%%BUILD%%/${BUILD}/g" \
        -i lib/Version.re

RUN eval $(opam env) && \
    dune runtest && \
    dune build bin/echoes.exe

FROM scratch
COPY --from=build /home/opam/app/_build/default/bin/echoes.exe /bin/echoes
ENTRYPOINT ["/bin/echoes"]
