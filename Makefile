
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help   -- show this help"
	@echo "  shell  -- nix develop"
	@echo "  build  -- cabal build all"
	@echo "  test   -- cabal test"
	@echo "  clean  -- cabal clean"
	@echo "  format -- format Haskell source files"

.PHONY: shell
shell:
	nix develop

.PHONY: build
build:
	cabal build all

.PHONY: test
test:
	cabal test plutarch-design-pattern-test --test-show-details=direct

.PHONY: clean
clean:
	cabal clean

.PHONY: format
format:
	fourmolu -i src
	fourmolu -i test
