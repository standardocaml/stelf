# Does README's install work on a clean Fedora? README asks for exactly two
# things, `make` and `opam`, and the Makefile provisions everything else -- so
# this installs those two and runs the documented targets, nothing more.
#
# From the repository root (its .dockerignore keeps host state out):
#   docker build --output type=cacheonly -f tools/ci/fedora.Dockerfile .
#
# cacheonly: the RUN steps are the test and the image is only their residue --
# ~7GB, which took five minutes of a nineteen-minute clean build just to
# export. To shell into the result, replace the --output flag with
# `-t stelf-ci:fedora`; every step is cached by then, so only the export runs.
FROM fedora

# make, plus what opam needs to build a compiler and fetch packages. Weak deps
# off is dnf's --no-install-recommends, and git-core is git without its Perl
# tools; diffutils is in Ubuntu's base image but not in Fedora's.
#
# opam: Fedora's own package, which is current (2.5.2 as of 2026-09).
RUN dnf install -y --setopt=install_weak_deps=False \
      bzip2 ca-certificates curl diffutils gcc git-core m4 make opam patch rsync unzip xz \
 && dnf clean all

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
