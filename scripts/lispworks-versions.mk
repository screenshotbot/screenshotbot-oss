# Links to all the Lispworks versions being used. By having a Makefile
# dependency on this file, we can essentially add a dependency on the
# Lispworks version.

LW_VERSION=8-0-0

LW=build/lw-console-$(LW_VERSION)$(ARCH)
LW_CORE=lispworks-unknown-location

LW_LIB_DIR=/opt/software/lispworks
PRIVATE_PATCH_DIR=$(LW_LIB_DIR)/lib/8-0-0-0/private-patches/
PRIVATE_PATCHES=$(call FIND,$(PRIVATE_PATCH_DIR),*.lisp)

ifeq ($(UNAME),Linux)
	LW_CORE=$(LW_PREFIX)/lispworks-$(LW_VERSION)-*-linux
endif

ifeq ($(UNAME),Darwin)
	LW_CORE=/Applications/LispWorks\ 8.0\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-8-0-0-macos64-universal
	LW_LIB_DIR=/Applications/LispWorks\ 8.0\ \(64-bit\)/Library
endif

ifeq ($(OS),Windows_NT)
	LW_CORE="C:\Program Files\LispWorks\lispworks-8-0-0-x64-windows.exe"
endif

