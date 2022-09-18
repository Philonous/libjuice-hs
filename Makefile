ifeq (${TERM},dumb)
export stack_args := ${stack_args} --colour=never
else
export stack_args := ${stack_args} --colour=auto
endif

srcfiles = $(shell find src -type f)
c-files = $(shell find libjuice -type f)

tests = dist/tests/tests

.PHONY: build
build: dist/libjuice-hs $(tests)

$(tests): dist/tests/% : dist/libjuice-hs $(test-srcfiles) libjuice-hs.cabal stack.yaml
	mkdir -p dist/tests
	cp "$(shell stack ${stack_args} path --dist-dir)/build/$(notdir $@)/$(notdir $@)" dist/tests/

dist/libjuice-hs: $(srcfiles) $(c-files) libjuice-hs.cabal stack.yaml
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

.PHONY: distclean
distclean: clean
	rm -rf .stack-work
