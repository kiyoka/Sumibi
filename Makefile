# Makefile for asset creation (e.g., GitHub releases)
#
# This Makefile infers the repository name and version from git tags or commit,
# and provides a target to package Emacs Lisp files under the lisp directory,
# Markdown documentation, and the LICENSE file into a tar.gz archive.
# You can override ARCHIVE_NAME on the command line.

# Repository name (directory name) and version (git tag or commit description)
REPO_NAME := $(shell basename $(shell git rev-parse --show-toplevel))
VERSION   := $(shell git describe --tags --always)

# Archive filename (override by specifying ARCHIVE_NAME)
ARCHIVE_NAME ?= $(REPO_NAME)-elisp-$(VERSION).tar.gz

# Locate all .el files under the lisp directory
EL_FILES := $(shell find ./lisp -type f -name '*.el')

# Markdown files in the repository root
MD_FILES := $(shell find . -maxdepth 1 -type f -name '*.md')

# LICENSE file, if present
LICENSE_FILE := $(wildcard LICENSE)

.PHONY: all release clean help

# Default target: create release
all: release

release: $(ARCHIVE_NAME)

# Package Emacs Lisp, Markdown, and LICENSE into tar.gz
$(ARCHIVE_NAME): $(EL_FILES) $(MD_FILES) $(LICENSE_FILE)
	@echo "Creating archive $@ containing Emacs Lisp, Markdown, and LICENSE files..."
	tar czf $@ $^

# Remove generated artifacts
clean:
	@echo "Removing generated archives..."
	@rm -f $(ARCHIVE_NAME)

# Display help
help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  all (default): same as 'release'"
	@echo "  release      : create $(ARCHIVE_NAME)"
	@echo "  clean        : remove generated archives" 
	@echo "  help         : show this message"
	@echo "  test         : run ERT unit tests in batch mode (requires Emacs)"

# --------------------------------------------------------------------
# Run Emacs ERT tests (Mozc tests are skipped if mozc.el is unavailable)
# --------------------------------------------------------------------
.PHONY: test

test:
	# Ensure mozc.el is visible by adding its system path to load-path.
	# If mozc がインストールされていない環境では、この -L オプションは無害です。
	emacs -batch -Q \
	      -L lisp \
      -L /usr/share/emacs/site-lisp/emacs-mozc \
      --eval "(require 'mozc nil 'noerror)" \
	      -l test/sumibi-mozc-tests.el \
	      -f ert-run-tests-batch-and-exit