TARGET         ?= stelf
DUNE_LOCK      ?= ./dune.lock/
OPAM_FILE      ?= ./stelf.opam
DUNE_BUILD_DIR ?= _build/default
DUNE_PROJECT   ?= ./dune-project
SWITCH         ?= .
OPAM           ?= opam
OPAM_EXEC      := $(OPAM) exec --switch $(SWITCH) --
DUNE           ?= dune
DOCKER         ?= docker

# Where `make install` puts the executable -- the only thing it installs.
BINDIR         ?= /usr/bin

SWITCH_SENTINEL := _opam/.opam-switch/switch-config
DUNE_SENTINEL   := _opam/lib/dune/META
DEPS_SENTINEL   := .deps-installed

.PHONY: all build test install uninstall docs clean check lock js ci

all: build

# Phase 1: initialize opam if needed, then create the local switch
$(SWITCH_SENTINEL):
	@$(OPAM) init --bare --no-setup --yes 2>/dev/null || true
	@$(OPAM) switch create $(SWITCH) --empty --yes 2>/dev/null || true
	@test -f $@ || (echo "ERROR: opam switch creation failed"; exit 1)

OPAM_DEPS = $(OPAM) install --switch $(SWITCH) --deps-only --with-test --with-doc --yes . ./basis

$(DUNE_SENTINEL): $(SWITCH_SENTINEL)
	@git submodule update --init --recursive
	@$(OPAM_DEPS)

# Phase 3: ensure submodules are present, then generate stelf.opam from dune-project
# dune.lock/ is committed — re-locking is done explicitly via `make lock`
$(OPAM_FILE): $(DUNE_PROJECT) $(DUNE_SENTINEL)
	@git submodule update --init --recursive
	$(OPAM_EXEC) $(DUNE) build $(OPAM_FILE)

# Phase 4: install whatever phase 2 did not. On a fresh switch that is nothing,
# and this is a quick no-op solve. It matters when a dune-project edit adds a
# dependency (phase 3 carries it into stelf.opam), and when phase 2 was cut
# short after dune itself went in -- dune's META is all its sentinel checks.
$(DEPS_SENTINEL): $(OPAM_FILE)
	@$(OPAM_DEPS)
	@touch $@

# Only the executable, straight into $(BINDIR): no lib/, no doc/, and no
# `dune build @install` tree to copy out of. `build` has already left a regular
# file at ./$(TARGET), so that is what gets copied.
#
# mkdir -p: on a fresh account ~/.local/bin need not exist, and cp will not
# create it.
#
# -f: dune's artifacts are read-only, and ./$(TARGET) and the installed copy
# inherit that 555. Without -f the FIRST install succeeds and every one after it
# dies with "cp: cannot create regular file ...: Permission denied", because cp
# cannot reopen what the previous install left. Same reason `build` below
# copies ./$(TARGET) with -f.
install: build
	@sudo cp -f ./$(TARGET) "$(BINDIR)/$(TARGET)"
	@echo "Installed $(TARGET) to $(BINDIR)/$(TARGET)"

# Removes exactly what `install` places.
uninstall:
	@rm -f "$(BINDIR)/$(TARGET)"
	@echo "Removed $(TARGET) from $(BINDIR)"

# Deliberately NOT dependent on `lock`: a normal build must not silently move
# dependency versions. Re-locking is explicit, via `make lock`.
build: $(DEPS_SENTINEL)
	$(OPAM_EXEC) $(DUNE) build
	@echo "Copying built executable to $(TARGET)"
	@cp -f $(DUNE_BUILD_DIR)/bin/main.exe ./$(TARGET)

test: $(DEPS_SENTINEL)
	$(OPAM_EXEC) $(DUNE) runtest

check: $(DEPS_SENTINEL)
	$(OPAM_EXEC) $(DUNE) build @check


docs: $(DEPS_SENTINEL)
	$(OPAM_EXEC) $(DUNE) build @doc

# Explicitly refresh the lock file (requires network; updates dune.lock/)
lock: $(DUNE_SENTINEL) $(DUNE_PROJECT)
	@$(OPAM_EXEC) $(DUNE) pkg lock

clean:
	$(OPAM_EXEC) $(DUNE) clean
	@rm -f ./$(TARGET) $(DEPS_SENTINEL)
CI_DOCKERFILES := $(sort $(wildcard tools/ci/*.Dockerfile))
CI_TARGETS     := $(patsubst tools/ci/%.Dockerfile,ci-%,$(CI_DOCKERFILES))
.PHONY: $(CI_TARGETS)

ci: $(CI_TARGETS)

$(CI_TARGETS): ci-%: tools/ci/%.Dockerfile
	$(DOCKER) build --output type=cacheonly -f $< .

.NOTPARALLEL: ci

