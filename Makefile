ifeq (${TERM},dumb)
export stack_args := ${stack_args} --colour=never
else
export stack_args := ${stack_args} --colour=auto
endif

srcfiles = $(shell find src -type f)
test-srcfiles = $(shell find test-suite -type f)

tests = dist/tests/tests

.PHONY: build
build: dist/libjuice-hs $(tests)

$(tests): dist/tests/% : dist/libjuice-hs $(test-srcfiles) package.yaml stack.yaml
	mkdir -p dist/tests
	cp "$(shell stack ${stack_args} path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist/tests/

libjuice:
	git submodule update --init --recursive

dist/libjuice-hs: libjuice $(srcfiles) $(test-srcfiles) package.yaml stack.yaml
	rm -f *.cabal
	rm -f stack.yaml.lock
	mkdir -p ./dist
	stack build --install-ghc \
	      --test --no-run-tests \
	      ${stack_args} \
	      ${stack_build_args} \
	      --copy-bins --local-bin-path ./dist

.PHONY: test
test: export TASTY_NUM_THREADS=1
test: $(tests)
	sh -c 'for t in dist/tests/*; do $$t; done'

.PHONY: clean
clean:
	stack clean
	rm -f stack.yaml.lock
	rm -rf dist
	rm -f *.cabal

.PHONY: distclean
distclean: clean
	rm -rf .stack-work
