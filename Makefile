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
# 名前にはこれまで通り "-elisp-" を挟むが、展開した際のディレクトリ階層は
# 例えば  ``Sumibi-3.1.0/…`` となるように、tar コマンドの --transform を使って
# ルートディレクトリを付与する。
ARCHIVE_NAME ?= $(REPO_NAME)-elisp-$(VERSION).tar.gz

# Directory prefix that will be embedded *inside* the archive so that users
# obtain   <repo>-<version>/   after extracting.
PACKAGE_DIR := $(REPO_NAME)-$(VERSION)

# Locate all .el files under the lisp directory
EL_FILES := $(shell find ./lisp -type f -name '*.el')

# Markdown files in the repository root (excluding GEMINI.md, AGENTS.md, CLAUDE.md, PLAN.md)
MD_FILES := $(shell find . -maxdepth 1 -type f -name '*.md' | grep -v -E 'GEMINI\.md|AGENTS\.md|CLAUDE\.md|PLAN\.md')

# LICENSE file, if present
LICENSE_FILE := $(wildcard LICENSE)

.PHONY: all release clean help

# Default target: create release
all: release

release: $(ARCHIVE_NAME)

# Package Emacs Lisp, Markdown, and LICENSE into tar.gz
# macOS の tar は --transform をサポートしないため、一時ディレクトリを使用する
$(ARCHIVE_NAME): $(EL_FILES) $(MD_FILES) $(LICENSE_FILE)
	@echo "Creating archive $@ containing Emacs Lisp, Markdown, and LICENSE files..."
	@rm -rf $(PACKAGE_DIR)
	@mkdir -p $(PACKAGE_DIR)/lisp
	@cp $(EL_FILES) $(PACKAGE_DIR)/lisp/
	@cp $(MD_FILES) $(LICENSE_FILE) $(PACKAGE_DIR)/
	tar czf $@ $(PACKAGE_DIR)
	@rm -rf $(PACKAGE_DIR)

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
	@echo "  release      : create $(ARCHIVE_NAME) (archive contents are placed under '$(PACKAGE_DIR)/')"
	@echo "  clean        : remove generated archives" 
	@echo "  help         : show this message"
	@echo "  test         : run ERT unit tests"

# --------------------------------------------------------------------
# Run Emacs ERT tests
# --------------------------------------------------------------------
.PHONY: test

test:
	@echo "Running tests..."
	emacs -batch -Q \
	      -L lisp \
	      -l test/sumibi-romaji-to-hiragana-test.el \
	      -f ert-run-tests-batch-and-exit