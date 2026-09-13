# Does README's install work on a clean Alpine? README asks for exactly two
# things, `make` and `opam`, and the Makefile provisions everything else -- so
# this installs those two and runs the documented targets, nothing more.
#
# From the repository root (its .dockerignore keeps host state out):
#   docker build --output type=cacheonly -f tools/ci/alpine.Dockerfile .
#
# cacheonly: the RUN steps are the test and the image is only their residue --
# ~7GB, which took five minutes of a nineteen-minute clean build just to
# export. To shell into the result, replace the --output flag with
# `-t stelf-ci:alpine`; every step is cached by then, so only the export runs.
FROM alpine

# make, plus what opam needs to build a compiler and fetch packages. build-base
# is Alpine's build-essential: gcc, musl-dev, make and patch. bash is in
# Ubuntu's base image but not Alpine's, and Jane Street's base has a dune rule
# that needs it: "I need bash to interpret (bash ...) actions".
#
# opam: Alpine's own package, which is current (2.5.2 as of 2026-09). It also
# brings in GNU tar and coreutils over busybox's, which is what opam expects.
RUN apk add --no-cache \
      bash build-base bzip2 ca-certificates curl git m4 make opam patch rsync unzip xz

# README's user is not root: opam balks at root, and `make install` targets $HOME.
# Alpine has busybox's adduser rather than useradd: -D skips the password, and
# the home directory and a stelf group come by default.
RUN adduser -D stelf
USER stelf
WORKDIR /home/stelf/stelf

# The one concession to Docker: bubblewrap cannot create namespaces inside
# `docker build`, so opam has to be initialized without its sandbox. The
# Makefile's own `opam init` then finds it already done.
RUN opam init --bare --disable-sandboxing --no-setup --yes

# --chown, or phase 2's `git submodule update` refuses a root-owned repository.
COPY --chown=stelf:stelf . .

RUN make check
RUN make build
# Non-blocking, as in build.yml: the suite is not yet green on a clean checkout.
RUN make test || echo "WARNING: make test failed (non-blocking)"
RUN make docs
RUN make install
