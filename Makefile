BUILD_DIR ?= _build/default
DUNE ?= dune

.PHONY: all build test docs install clean repl check help

all: build test docs install 


build:
	@$(DUNE) build


check: 
	@$(DUNE) build @check

repl:
	@$(DUNE) utop

test:
	@$(DUNE) runtest

docs:
	@$(DUNE) build @doc

install:
	@$(DUNE) install


clean:
	@$(DUNE) clean

