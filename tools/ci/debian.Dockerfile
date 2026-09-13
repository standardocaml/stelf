# Does README's install work on a clean Debian? README asks for exactly two
# things, `make` and `opam`, and the Makefile provisions everything else -- so
# this installs those two and runs the documented targets, nothing more.
#
# From the repository root (its .dockerignore keeps host state out):
#   docker build --output type=cacheonly -f tools/ci/debian.Dockerfile .
#
# cacheonly: the RUN steps are the test and the image is only their residue --
# ~7GB, which took five minutes of a nineteen-minute clean build just to
# export. To shell into the result, replace the --output flag with
# `-t stelf-ci:debian`; every step is cached by then, so only the export runs.
FROM debian

ARG DEBIAN_FRONTEND=noninteractive

# make, plus what opam needs to build a compiler and fetch packages.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      build-essential bzip2 ca-certificates curl git m4 make patch rsync unzip xz-utils \
 && rm -rf /var/lib/apt/lists/*

# opam: the pinned upstream binary (trixie's apt package is 2.3.0).
ARG OPAM_VERSION=2.5.2
ARG TARGETARCH=amd64
RUN case "${TARGETARCH}" in \
      amd64) opam_arch=x86_64 ;; \
      arm64) opam_arch=arm64  ;; \
      *) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac \
 && curl -fsSL -o /usr/local/bin/opam \
      "https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-${opam_arch}-linux" \
 && chmod 755 /usr/local/bin/opam

# README's user is not root: opam balks at root, and `make install` targets $HOME.
RUN useradd --create-home stelf
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
