# Copyright 2018-Present Modern Interpreters Inc.
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

SBCL_C_FLAGS=--dynamic-space-size 2048
sbcl=build/sbcl-console
CACHE_KEY=10
SBCL_CORE=sbcl $(SBCL_C_FLAGS) --no-userinit
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

LW_PREFIX=/opt/software/lispworks

ifeq ($(UNAME),Linux)
define timeout
timeout $1
endef
define timeout
endef
else



endif

define wild_src
$(call FIND,$(SRC_DIRS), $(1))
endef

JIPR=../jippo
SRC_DIRS=src local-projects third-party scripts quicklisp
LISP_FILES=$(call wild_src, '*.lisp') $(call wild_src, '*.asd') $(call will_src,'*.c') $(call wild_src,'*.cpp')

JS_FILES=$(call wild_src, '*.js')
CSS_FILES=$(call wild_src, '*.css') $(call wild_src, '*.scss')

LW_SCRIPT=PATH=/opt/homebrew/bin:$$PATH $(call timeout,15m) $(LW) -quiet -build
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
	TOUCH=powershell -command New-Item
	FIND=$(shell gci -r $2 | Select FullName)
	RM=echo
else
	UNAME=$(shell uname -s)
	MKDIR=mkdir -p
	TOUCH=touch
	FIND=$(shell find $(1) -name $(2))
	RM=rm -f
endif

CYGWIN=$(findstring CYGWIN,$(UNAME))
DISTINFO=quicklisp/dists/quicklisp/distinfo.txt
ARC=build/arc/bin/arc

REVISION_ID=$(shell echo '{"ids":["$(DIFF_ID)"]}' | $(ARC) call-conduit differential.querydiffs -- | jq -r '.["response"]["$(DIFF_ID)"]["revisionID"]')
QUICKLISP_DEPS=$(call FIND,quicklisp,'*.txt') $(call FIND,quicklisp,'*.lisp')
IMAGE_DEPS=scripts/build-image.lisp scripts/asdf.lisp $(DISTINFO) scripts/prepare-image.lisp scripts/init.lisp scripts/asdf.lisp $(QUICKLISP_DEPS)

define ht_check_tests
	TMP=$(TMPFILE) && $1 | tee  $$TMP &&  grep "all tests PASSED" $$TMP && rm $$TMP
endef

include scripts/lispworks-versions.mk

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
	$(sbcl) $(SBCL_C_FLAGS) --script ./scripts/jenkins.lisp

test-ccl: submodule $(CCL_IMAGE)
	$(CCL_SCRIPT) ./scripts/jenkins.lisp

test-lw: submodule $(LW) build/affected-files.txt
	$(LW_SCRIPT) ./scripts/jenkins.lisp

test-store: submodule $(LW)
	$(LW_SCRIPT) ./scripts/run-store-tests.lisp

build/.keep: | build/cache-key $(DISTINFO)
	$(TOUCH) $@

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

screenshotbot-tests: $(LW) .PHONY
	$(LW_SCRIPT) ./scripts/jenkins.lisp -system screenshotbot/tests,screenshotbot.pro/tests,screenshotbot.sdk/tests

sdk-tests: $(LW) .PHONY
	$(LW_SCRIPT) ./scripts/jenkins.lisp -system screenshotbot.sdk/tests -no-jvm

$(LW): build/.keep $(IMAGE_DEPS) 
	echo in here
# $$PWD is workaround over LW issue #42471
	$(ARCH_CMD) $(LW_CORE) -build scripts/build-image.lisp
	test -f $(LW)

$(sbcl): build/.keep $(IMAGE_DEPS) .PHONY Makefile
	$(SBCL_CORE) --script scripts/build-image.lisp

selenium-tests: $(LW)
	# rm -rf src/screenshotbot/selenium-output
	# $(LW_SCRIPT) scripts/run-selenium-tests.lisp

selenium-tests-without-x: $(LW)
	$(LW_SCRIPT) scripts/run-selenium-tests.lisp

$(CCL_IMAGE): build/.keep $(IMAGE_DEPS)
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
	SCREENSHOTBOT_CLI_V2=1 ~/screenshotbot/recorder ci static-website --directory src/screenshotbot/static-web-output/ --main-branch master --channel screenshotbot-oss --repo-url 'git@github.com:screenshotbot/screenshotbot-oss.git' --main-branch main

emacs-tests:
	cd src/emacs && make all
