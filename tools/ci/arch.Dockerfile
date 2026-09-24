# Does README's install work on a clean Arch Linux? README asks for exactly two
# things, `make` and `opam`, and the Makefile provisions everything else -- so
# this installs those two and runs the documented targets, nothing more.
#
# From the repository root (its .dockerignore keeps host state out):
#   docker build --output type=cacheonly -f tools/ci/arch.Dockerfile .
#
# cacheonly: the RUN steps are the test and the image is only their residue --
# ~7GB, which took five minutes of a nineteen-minute clean build just to
# export. To shell into the result, replace the --output flag with
# `-t stelf-ci:arch`; every step is cached by then, so only the export runs.
FROM archlinux

# make, plus what opam needs to build a compiler and fetch packages. -Syu, not
# -Sy: Arch does not support partial upgrades. gcc brings binutils and glibc
# ships its own headers, so gcc and make are the whole of build-essential here;
# diffutils is in Ubuntu's base image but not in Arch's.
#
# opam: Arch's own package, which is current (2.5.2 as of 2026-09). It pulls in
# a system OCaml through ocaml-compiler-libs, which the Makefile's local switch
# does not use -- that switch builds its own compiler.
#
# The cache is removed by hand: `pacman -Scc --noconfirm` answers its own
# prompt with the default, which is to keep everything.
RUN pacman -Syu --noconfirm --needed \
      bzip2 ca-certificates curl diffutils gcc git m4 make opam patch rsync unzip xz \
 && rm -rf /var/cache/pacman/pkg/*

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
