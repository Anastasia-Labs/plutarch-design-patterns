
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help  -- show this help"
	@echo "  shell -- nix develop"
	@echo "  build -- cabal build"
	@echo "  test  -- cabal test"
	@echo "  clean -- cabal clean"

.PHONY: shell
shell:
	nix develop

.PHONY: build
build:
	cabal build

.PHONY: test
test:
	cabal test

.PHONY: clean
clean:
	cabal clean
