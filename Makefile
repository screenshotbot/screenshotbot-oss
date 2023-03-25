# Copyright 2018-Present Modern Interpreters Inc.
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

sbcl=build/sbcl-console
CACHE_KEY=7
SBCL_CORE=sbcl --no-userinit
CCL_DEFAULT_DIRECTORY?=/opt/software/ccl
CCL_CORE=$(CCL_DEFAULT_DIRECTORY)/lx86cl64
CCL_IMAGE=build/ccl-console
tests= \
	./test-stuff.lisp \
	./test-stuff2.lisp

ARCH?=
ARCH_CMD=

ifeq ($(ARCH),x86_64)
	ARCH_CMD=arch -$(ARCH)
endif

LW_VERSION=8-0-0
LW_PREFIX=/opt/software/lispworks
QUICKLISP_DEPS=quicklisp/dists/quicklisp/distinfo.txt \
    quicklisp/dists/quicklisp/releases.txt \
	quicklisp/dists/quicklisp/systems.txt


ifeq ($(UNAME),Linux)
define timeout
timeout $1
endef
define timeout
endef
else



endif

JIPR=../jippo
LW=build/lw-console-$(LW_VERSION)$(ARCH)
LW_CORE=lispworks-unknown-location
SRC_DIRS=src local-projects third-party
LISP_FILES=$(shell find $(SRC_DIRS) -name '*.lisp') $(shell find $(SRC_DIRS) -name '*.asd')
LW_SCRIPT=$(call timeout,15m) $(LW) -quiet -build
SBCL_SCRIPT=$(sbcl) --script
TMPFILE=$(shell mktemp)
JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
SBCL_SCRIPT=$(call timeout,5m) $(sbcl) --script
CCL_SCRIPT=CCL_DEFAULT_DIRECTORY=$(CCL_DEFAULT_DIRECTORY) $(CCL_CORE) -b -I $(CCL_IMAGE)

QUICKLISP=quicklisp/dists/quicklisp/
COPYBARA_CMD=java -jar scripts/copybara_deploy.jar

define COPYBARA
( $(COPYBARA_CMD) copy.bara.sky $1 | tee build/cb-output ) || grep "No new changes to import" build/cb-output
endef

ifeq ($(OS), Windows_NT)
	UNAME=Windows
	MKDIR=mkdir -pf
else
	UNAME=$(shell uname -s)
	MKDIR=mkdir -p
endif

CYGWIN=$(findstring CYGWIN,$(UNAME))
DISTINFO=quicklisp/dists/quicklisp/distinfo.txt
ARC=build/arc/bin/arc

REVISION_ID=$(shell echo '{"ids":["$(DIFF_ID)"]}' | $(ARC) call-conduit differential.querydiffs -- | jq -r '.["response"]["$(DIFF_ID)"]["revisionID"]')
QUICKLISP_DEPS=$(shell find quicklisp -name '*.txt') $(shell find quicklisp -name '*.lisp')
IMAGE_DEPS=scripts/build-image.lisp scripts/asdf.lisp $(DISTINFO) scripts/prepare-image.lisp scripts/init.lisp scripts/asdf.lisp $(QUICKLISP_DEPS)

ifeq ($(UNAME),Linux)
	LW_CORE=$(LW_PREFIX)/lispworks-$(LW_VERSION)-amd64-linux
endif

ifeq ($(UNAME),Darwin)
	LW_CORE=/Applications/LispWorks\ 8.0\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-8-0-0-macos64-universal
endif

ifeq ($(OS),Windows_NT)
	LW_CORE="C:\Program Files\LispWorks\lispworks-8-0-0-x64-windows.exe"
endif

define ht_check_tests
	TMP=$(TMPFILE) && $1 | tee  $$TMP &&  grep "all tests PASSED" $$TMP && rm $$TMP
endef

ifneq ("$(wildcard scripts/common.mk)", "")
	include scripts/common.mk
endif

all:
	true

submodule:
#	git submodule init
# git submodule update



build/cache-key: .PHONY
ifneq ($(OS),Windows_NT)
	if ! [ -e build/cache-key ] || ! [ x`cat build/cache-key` = x$(CACHE_KEY) ] ; then \
		echo "Cleaning build/ directory" ; \
		rm -rf build/asdf-cache build/slime-fasls ; \
		rm -rf quicklisp/dists/quicklisp/software ; \
		mkdir build ; \
		echo $(CACHE_KEY) > $@ ; \
	fi
else
# TODO: detect the cache-key correctly, currently you have to manually delete this file to reset
	if not exist build mkdir build
	if not exist $@ ( echo $(CACHE_KEY) > $@ )
endif


.PHONY:

update-quicklisp: .PHONY
	$(SBCL_CORE) --eval '(load "quicklisp/setup.lisp")' --eval '(ql:update-all-dists :prompt nil)' --quit

update-quicklisp-client: .PHONY
	$(SBCL_CORE) --eval '(load "quicklisp/setup.lisp")' --eval '(ql:update-client :prompt nil)' --quit


start-dev: $(sbcl)
	$(sbcl) --script ./start-dev.lisp

install-dep: install-dep.lisp $(sbcl)
	$(sbcl) --script ./install-dep.lisp

show-info:
	git status
	env

clean-sys-index:
	rm -f system-index.txt
	rm -rfv local-projects/quicklisp
	rm -rfv */system-index.txt
	rm -f quicklisp/local-projects/system-index.txt

tests:| show-info clean-sys-index test-parts selenium-tests conditional-copybara

test-parts: test-sb test-lw test-ccl test-store

test-sb: submodule $(sbcl) build/affected-files.txt
	$(sbcl) --script ./scripts/jenkins.lisp

test-ccl: submodule $(CCL_IMAGE)
	$(CCL_SCRIPT) ./scripts/jenkins.lisp

test-lw: submodule $(LW) build/affected-files.txt
	$(LW_SCRIPT) ./scripts/jenkins.lisp

test-store: submodule $(LW)
	$(LW_SCRIPT) ./run-store-tests.lisp

build: | build/cache-key $(DISTINFO)
# build/build? Temporary fix for LW8-darwin
	$(MKDIR) build

clean-fasl: .PHONY
	find src -name *.64ufasl -delete

clean: clean-fasl
	rm -rf build

hunchentoot-tests-sb: $(sbcl)
	$(call ht_check_tests,$(SBCL_SCRIPT) scripts/run-hunchentoot-tests.lisp)

hunchentoot-tests-lw: $(LW)
	$(call ht_check_tests,$(LW_SCRIPT) scripts/run-hunchentoot-tests.lisp)

screenshotbot-flow:

	adb shell settings put global hidden_api_policy_p_apps 1
	adb shell settings put global hidden_api_policy_pre_p_apps 1
	adb shell settings put global hidden_api_policy 1
	cd ~/builds/silkwrmsdk && ./gradlew :core:publishToMavenLocal :plugin:publishToMavenLocal
	#	cd ~/builds/screenshotbot-example && ./gradlew :connectedDebugAndroidTest
	cd ~/builds/screenshotbot-example && ./gradlew -i :debugAndroidTestScreenshotbot

bknr-tests-lw:

screenshotbot-tests: $(LW) .PHONY
	$(LW_SCRIPT) ./scripts/jenkins.lisp -system screenshotbot/tests,screenshotbot.pro/tests,screenshotbot.sdk/tests

sdk-tests: $(LW) .PHONY
	$(LW_SCRIPT) ./scripts/jenkins.lisp -system screenshotbot.sdk/tests -no-jvm

$(LW): build $(IMAGE_DEPS)
	echo in here
# $$PWD is workaround over LW issue #42471
	$(ARCH_CMD) $(LW_CORE) -build scripts/build-image.lisp

$(sbcl): build $(IMAGE_DEPS) .PHONY
	$(SBCL_CORE) --script scripts/build-image.lisp

selenium-tests: $(LW)
	# rm -rf src/screenshotbot/selenium-output
	# $(LW_SCRIPT) scripts/run-selenium-tests.lisp

selenium-tests-without-x: $(LW)
	$(LW_SCRIPT) scripts/run-selenium-tests.lisp

$(CCL_IMAGE): build $(IMAGE_DEPS)
	rm -f $@
	$(CCL_CORE) -l scripts/build-image.lisp
	chmod a+x $@


update-ip: $(sbcl)
	$(SBCL_SCRIPT) update-ip.lisp

copybara: .PHONY build
	# This is on arnold's jenkins server. Disregard for OSS use.
	ssh-add ~/.ssh/id_rsa_screenshotbot_oss
	$(call COPYBARA)

copybara-slite: .PHONY build
	ssh-add ~/.ssh/id_rsa_slite
	$(call COPYBARA,slite)


copybara-quick-patch: .PHONY build
	ssh-add ~/.ssh/id_rsa_quick_patch
	$(call COPYBARA,quick-patch)

conditional-copybara: validate-copybara
	if [ x$$DIFF_ID = x ] ; then \
	   make all-copybara ; \
	fi

all-copybara:
	ssh-agent $(MAKE) copybara
	ssh-agent $(MAKE) copybara-slite
	ssh-agent $(MAKE) copybara-quick-patch

validate-copybara: .PHONY
	$(COPYBARA_CMD) validate copy.bara.sky

update-harbormaster-pass: $(sbcl)
	$(SBCL_SCRIPT) ./scripts/update-phabricator.lisp pass

update-harbormaster-fail: $(sbcl)
	$(SBCL_SCRIPT) ./scripts/update-phabricator.lisp fail


autoland:
	if ( echo "{\"revision_id\":\"$(REVISION_ID)\"}" | $(ARC) call-conduit differential.getcommitmessage -- | grep "#autoland" ) ; then \
	 	$(MAKE) -s actually-land ; \
	fi

actually-land:
	git status
	echo "Landing..."
	$(ARC) land  --

src/java/libs: .PHONY
	cd src/java && make libs

upload-mac-intel-sdk:
	ARCH="x86_64" make upload-sdk


upload-screenshots-oss: .PHONY
	curl https://screenshotbot.io/recorder.sh | bash
	~/screenshotbot/recorder --static-website src/screenshotbot/static-web-output/ --main-branch master --channel screenshotbot-oss --repo-url 'git@github.com:screenshotbot/screenshotbot-oss.git' --main-branch main
