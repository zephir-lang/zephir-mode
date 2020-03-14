# Copyright (C) 2017, 2018, 2019, 2020 Serghei Iakovlev
#
# This file is not part of GNU Emacs.
#
# License
#
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.

include default.mk

# Run “make build” by default
.DEFAULT_GOAL = build

ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
TESTFLAGS ?=
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(RUNEMACS) --eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $<

## Public targets

.PHONY: .title
.title:
	$(info Zepphir Mode $(VERSION))

.PHONY: init
init: Cask
	@$(CASK) install

.PHONY: checkdoc
checkdoc:
	@$(EMACSBATCH) --eval '(checkdoc-file "$(SRCS)")'
	$(info Done.)

.PHONY: build
build: $(OBJS)

.PHONY: test
test:
	@$(CASK) exec ert-runner $(TESTFLAGS)

.PHONY: clean
clean:
	$(info Remove all byte compiled Elisp files...)
	@$(CASK) clean-elc
	$(info Remove build artefacts...)
	@$(RM) README ChangeLog coverage-final.json
	@$(RM) $(PACKAGE)-pkg.el $(PACKAGE)-*.tar

.PHONY: help
help: .title
	@echo ''
	@echo 'Run "make init" first to install and update all local dependencies.'
	@echo 'See "default.mk" for variables you might want to set.'
	@echo ''
	@echo 'Available targets:'
	@echo '  help:       Show this help and exit'
	@echo '  init:       Initialize the project (has to be launched first)'
	@echo '  checkdoc:   Checks Zephir Mode code for errors in the documentation'
	@echo '  build:      Byte compile Zephir Mode package'
	@echo '  test:       Run the non-interactive unit test suite'
	@echo '  clean:      Remove all byte compiled Elisp files, documentation,'
	@echo '              build artefacts and tarball'
	@echo ''
	@echo 'Available programs:'
	@echo '  $(CASK): $(if $(HAVE_CASK),yes,no)'
	@echo ''
	@echo 'You need $(CASK) to develop Zephir Mode.'
	@echo 'See http://cask.readthedocs.io/ for more.'
	@echo ''
