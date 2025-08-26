EMACS ?= emacs
EMACSFLAGS = -Q -batch -L .
GPTEL_DIR ?= ~/ghq/github.com/karthink/gptel

SRCS = gptel-fabric.el
TESTS = test-gptel-fabric.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: all compile test clean lint check install test-standalone

all: compile

compile: $(OBJECTS)

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) -L $(GPTEL_DIR) -f batch-byte-compile $<

test:
	$(EMACS) $(EMACSFLAGS) -L $(GPTEL_DIR) -l ert -l $(TESTS) -f ert-run-tests-batch-and-exit

test-standalone:
	@echo "Running tests without gptel dependency (mock mode)..."
	$(EMACS) $(EMACSFLAGS) -l ert -l test-gptel-fabric-standalone.el -f ert-run-tests-batch-and-exit

lint:
	$(EMACS) $(EMACSFLAGS) --eval "(progn \
		(require 'checkdoc) \
		(checkdoc-file \"gptel-fabric.el\") \
		(kill-emacs))"

package-lint:
	$(EMACS) $(EMACSFLAGS) --eval "(progn \
		(package-initialize) \
		(unless (package-installed-p 'package-lint) \
			(package-refresh-contents) \
			(package-install 'package-lint)) \
		(require 'package-lint) \
		(package-lint-batch-and-exit \"gptel-fabric.el\"))"

check: lint test

clean:
	rm -f *.elc

install: compile
	@echo "Installing gptel-fabric..."
	@echo "Please add the following to your Emacs configuration:"
	@echo "(add-to-list 'load-path \"$(PWD)\")"
	@echo "(require 'gptel-fabric)"

help:
	@echo "gptel-fabric Makefile targets:"
	@echo "  make compile       - Byte compile elisp files"
	@echo "  make test         - Run test suite"
	@echo "  make lint         - Check documentation strings"
	@echo "  make package-lint - Run package-lint checks"
	@echo "  make check        - Run lint and tests"
	@echo "  make clean        - Remove compiled files"
	@echo "  make install      - Show installation instructions"
	@echo "  make help         - Show this help message"