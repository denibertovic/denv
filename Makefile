.PHONY: build repl test release help nix-build

.DEFAULT_GOAL = help

VERSION ?= $(shell grep "^version:" denv.cabal | cut -d " " -f9)
STATIC_BUILD_SCRIPT ?= $(shell nix-build --no-link -A fullBuildScript static.nix)

## Run build
build:
	@stack build

## Run nix-build
nix-build:
	@nix-build -E 'with import <nixpkgs> { }; callPackage ./default.nix { }' -A denv.components.exes.denv

## Run repl
repl:
	@stack repl

## Run tests. Example RUN_INTEGRATION_TESTS=1 make test
test:
	@RUN_INTEGRATION_TESTS=1 stack test

## Cut new release
release:
	@git tag ${VERSION} && git push --tags

## Build static binary with nix
static:
	@$(STATIC_BUILD_SCRIPT) static.nix

## Run ghcid
ghcid:
	@ghcid \
		--command "stack ghci denv --ghci-options=-fno-code"

## Have ghcid run the test suite on successful recompile
ghcid-test:
	@ghcid \
		--command "stack ghci denv:lib denv:test:hspec --ghci-options=-fobject-code" \
		--test "main"

## Print current version
version:
	@echo ${VERSION}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\0-9_]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
