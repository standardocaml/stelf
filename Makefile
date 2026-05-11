FLAGS += 
DUNE ?= dune
OPAM ?= opam
SRCS=
OUTPUT_DIR ?= _build
OUTPUT ?= build

.PHONY: default run build test fmt install check docs repl
default: install

check: dune dune-project dune-workspace $(SRCS)
	@$(DUNE) build --profile=check 
	@cp $(OUTPUT_DIR)/default/bin/main.exe $(OUTPUT)/stelf.exe
build: dune dune-project dune-workspace $(SRCS)
	@mkdir -p $(OUTPUT)
	@$(DUNE) build --profile=release
	@cp $(OUTPUT_DIR)/default/bin/main.exe $(OUTPUT)/stelf.exe
test: dune dune-project dune-workspace $(SRCS)
	@$(DUNE) build --profile=dev test/integration_runner.exe test/unit_runner.exe
	@ret=0; \
	$(DUNE) exec --profile=dev -- ./test/integration_runner.exe || ret=$$?; \
	$(DUNE) exec --profile=dev -- ./test/unit_runner.exe || ret=$$?; \
	exit $$ret

repl: dune dune-project dune-workspace $(SRCS)
	@$(DUNE) exec --profile=dev bin/main.exe

fmt: dune dune-project dune-workspace $(SRCS)
	@$(DUNE) fmt 


install: build-release 
	@$(DUNE) build @install
	@$(DUNE) install
