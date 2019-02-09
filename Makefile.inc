#
# Makefile containing common configuration for inclusion in other Makefiles
#
# Author: Uli Schlachter <psychon@znc.in>
# License: MIT
#

PREFIX = /usr/local
LUA_VERSION=5.1
LUA_LIBDIR = $(PREFIX)/lib/lua/$(LUA_VERSION)
LUA_SHAREDIR = $(PREFIX)/share/lua/$(LUA_VERSION)
LUA = lua
PKG_CONFIG = pkg-config

HOST_OS = $(shell uname -s | tr A-Z a-z)

ifneq ($(filter cygwin% msys% mingw%, $(HOST_OS)),)
EXT = .dll
PFX = cyg
LIBFLAG = -shared
LIBS += -llua
else
ifeq ($(HOST_OS),darwin)
EXT = .so
PFX = lib
LIBFLAG = -bundle -undefined dynamic_lookup
CCSHARED = -fno-common
else
EXT = .so
PFX = lib
LIBFLAG = -shared
CCSHARED = -fPIC
endif
endif

ifndef CFLAGS
ifndef COPTFLAGS
CFLAGS = -Wall -Wextra -O2 -g
endif
endif
