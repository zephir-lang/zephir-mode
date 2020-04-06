# Copyright (C) 2017-2020 Free Software Foundation, Inc
#
# This file is NOT part of GNU Emacs.
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

# Run “make build” by default
.DEFAULT_GOAL = build

EMACS  ?= emacs
CASK   ?= cask

EMACSFLAGS ?=
TESTFLAGS  ?= -L .

EMACSBATCH = $(EMACS) -Q --batch -L . $(EMACSFLAGS)
RUNEMACS   =

# Program availability
HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  Please run make help")
RUNEMACS = $(EMACSBATCH)
else
RUNEMACS = $(CASK) exec $(EMACSBATCH)
endif

VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' zephir-mode.el)"

PACKAGE = zephir-mode
ARCHIVE_NAME = $(PACKAGE)-$(VERSION)

# File lists
AUTOLOADS = zephir-mode-autoloads.el
SRCS = zephir.el zephir-mode.el zephir-face.el zephir-indent.el
OBJS = $(SRCS:.el=.elc)
