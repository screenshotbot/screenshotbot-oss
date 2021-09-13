# Copyright 2018-Present Modern Interpreters Inc.
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

sbcl=build/sbcl-console
CACHE_KEY=5
SBCL_CORE=sbcl
CCL_DEFAULT_DIRECTORY=/opt/software/ccl
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

JIPR=../jippo
LW=build/lw-console-$(LW_VERSION)$(ARCH)
LW_CORE=lispworks-unknown-location
SRC_DIRS=src local-projects third-party
LISP_FILES=$(shell find $(SRC_DIRS) -name '*.lisp') $(shell find $(SRC_DIRS) -name '*.asd')
LW_SCRIPT=timeout 15m $(LW) -quiet -build
SBCL_SCRIPT=$(sbcl) --script
TMPFILE=$(shell mktemp)
JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
SBCL_SCRIPT=timeout 5m $(sbcl) --script
CCL_SCRIPT=CCL_DEFAULT_DIRECTORY=$(CCL_DEFAULT_DIRECTORY) $(CCL_CORE) -b -I $(CCL_IMAGE)

QUICKLISP=quicklisp/dists/quicklisp/
COPYBARA_CMD=java -jar scripts/copybara_deploy.jar

define COPYBARA
( $(COPYBARA_CMD) copy.bara.sky $1 | tee build/cb-output ) || grep "No new changes to import" build/cb-output
endef


UNAME=$(shell uname -s)
DISTINFO=quicklisp/dists/quicklisp/distinfo.txt
ARC=build/arc/bin/arc

REVISION_ID=$(shell echo '{"ids":["$(DIFF_ID)"]}' | $(ARC) call-conduit differential.querydiffs -- | jq -r '.["response"]["$(DIFF_ID)"]["revisionID"]')
IMAGE_DEPS=scripts/build-image.lisp scripts/asdf.lisp $(DISTINFO) scripts/prepare-image.lisp scripts/init.lisp

ifeq ($(UNAME),Linux)
	LW_CORE=$(LW_PREFIX)/lispworks-$(LW_VERSION)-amd64-linux
endif

ifeq ($(UNAME),Darwin)
	LW_CORE=/Applications/LispWorks\ 8.0\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-8-0-0-macos64-universal
endif


define clsql_check_tests
	TMP=$(TMPFILE) && $1 | tee  $$TMP &&  ! ( grep  "total tests failed" $$TMP ) && grep "Finished Running Tests Against" $$TMP && rm $$TMP
endef

define ht_check_tests
	TMP=$(TMPFILE) && $1 | tee  $$TMP &&  grep "all tests PASSED" $$TMP && rm $$TMP
endef

all:
	true

submodule:
	#	git submodule init
	# git submodule update

build/cache-key: .PHONY
	if ! [ -e build/cache-key ] || ! [ x`cat build/cache-key` = x$(CACHE_KEY) ] ; then \
		echo "Cleaning build/ directory" ; \
		rm -rf build/asdf-cache build/slime-fasls ; \
		rm -rf quicklisp/dists/quicklisp/software ; \
		mkdir build ; \
		echo $(CACHE_KEY) > $@ ; \
	fi

.PHONY:

update-quicklisp: .PHONY $(sbcl)
	# $(sbcl) --eval '(load "quicklisp/setup.lisp")' --eval '(ql:update-client :prompt nil)'  --quit
	$(MAKE) $(sbcl)
	$(sbcl) --eval '(load "quicklisp/setup.lisp")' --eval '(ql:update-all-dists :prompt nil)' --quit

start-dev: $(sbcl)
	$(sbcl) --script ./start-dev.lisp

install-dep: install-dep.lisp $(sbcl)
	$(sbcl) --script ./install-dep.lisp

show-info:
	git status
	env

clean-sys-index:
	rm -f system-index.txt
	rm -rf local-projects/quicklisp
	rm -rf */system-index.txt

tests:| show-info clean-sys-index test-parts selenium-tests conditional-copybara

test-parts: test-sb test-lw test-ccl test-store web-bin

test-sb: submodule $(sbcl)
	pwd
	$(sbcl) --script ./jenkins.lisp

test-ccl: submodule $(CCL_IMAGE)
	pwd
	$(CCL_SCRIPT) ./jenkins.lisp

test-lw: submodule $(LW)
	pwd
	$(LW_SCRIPT) ./jenkins.lisp

test-store: submodule $(LW)
	$(LW_SCRIPT) ./run-store-tests.lisp

deploy-jipr:
	cd $(JIPR) && buck build jipr
	cd $(JIPR) && cp `buck targets --show-output jipr | cut -d ' ' -f 2`  $(PWD)/jipr/static/binary/jipr.jar

restart: | test-lw  web-bin
	kill -9 `curl https://tdrhq.com/deploy/getpid`

build: | build/cache-key $(DISTINFO)
	# build/build? Temporary fix for LW8-darwin
	mkdir -p build

web-bin: $(LISP_FILES) $(LW)
	$(LW_SCRIPT) build-web-bin.lisp

clean-fasl: .PHONY
	find src -name *.64ufasl -delete

clean: clean-fasl
	rm -f web-bin
	rm -rf buil
	rm -rf assets

clsql-tests-sbcl: $(sbcl)
	$(call clsql_check_tests,$(SBCL_SCRIPT) scripts/run-clsql-tests.lisp)

clsql-tests-lw: $(LW)
	$(call clsql_check_tests,$(LW_SCRIPT) scripts/run-clsql-tests.lisp)

hunchentoot-tests-sb: $(sbcl)
	$(call ht_check_tests,$(SBCL_SCRIPT) scripts/run-hunchentoot-tests.lisp)

hunchentoot-tests-lw: $(LW)
	$(call ht_check_tests,$(LW_SCRIPT) scripts/run-hunchentoot-tests.lisp)

clsql-tests: | clsql-tests-sbcl clsql-tests-lw

screenshotbot-flow:

	adb shell settings put global hidden_api_policy_p_apps 1
	adb shell settings put global hidden_api_policy_pre_p_apps 1
	adb shell settings put global hidden_api_policy 1
	cd ~/builds/silkwrmsdk && ./gradlew :core:publishToMavenLocal :plugin:publishToMavenLocal
	#	cd ~/builds/screenshotbot-example && ./gradlew :connectedDebugAndroidTest
	cd ~/builds/screenshotbot-example && ./gradlew -i :debugAndroidTestScreenshotbot

build/deploy.tar.gz: web-bin .PHONY
	rm -f $@
	tar cvzf $@ build/web-bin-delivered screenshotbot java/build/libs/

bknr-tests-lw:

screenshotbot-tests: $(LW) .PHONY
	$(LW_SCRIPT) ./jenkins.lisp -system screenshotbot/tests,screenshotbot.pro/tests,screenshotbot.sdk/tests

sdk-tests: $(LW) .PHONY
	$(LW_SCRIPT) ./jenkins.lisp -system screenshotbot.sdk/tests -no-jvm

deploy-rsync: .PHONY web-bin
	rsync -aPz --exclude .git --exclude .web-bin-copy --exclude web-bin ./ ubuntu@mx.tdrhq.com:~/web/


$(LW): build $(IMAGE_DEPS)
	# $$PWD is workaround over LW issue #42471
	$(ARCH_CMD) $(LW_CORE) -build scripts/build-image.lisp -- $$PWD/$@

$(sbcl): build $(IMAGE_DEPS)
	$(SBCL_CORE) --script scripts/build-image.lisp


selenium-tests: $(LW)
	xvfb-run -a $(LW_SCRIPT) scripts/run-selenium-tests.lisp

selenium-tests-without-x: $(LW)
	$(LW_SCRIPT) scripts/run-selenium-tests.lisp

assets: $(LW) .PHONY
	mkdir -p assets
	rm -f assets/lispcalls.jar
	cp $(LW_PREFIX)/lib/$(LW_VERSION)-0/etc/lispcalls.jar assets/
	$(LW_SCRIPT) scripts/deliver-screenshotbot.lisp

deploy-assets: | deploy-assets-excl-bin deploy-bin deploy-pull

deploy-assets-excl-bin: assets
	rsync --checksum -aPz --exclude screenshotbot assets web@screenshotbot.io:~/web/

deploy-bin: assets
	rsync -aPz assets/screenshotbot web@screenshotbot.io:~/web/assets/screenshotbot-copy
	ssh web@screenshotbot.io mv '~/web/assets/screenshotbot-copy' '~/web/assets/screenshotbot'

deploy-pull:
	ssh web@screenshotbot.io 'cd web && git pull'

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
	# This is manually run, not meant to run during build
	rm -rf $@
	mkdir -p $@
	cd java && ./gradlew build makeClasspath
	cp scripts/java-jar-prefix.txt $@/java.libs.asd

	for x in `cat java/build/libs/classpath.txt` ; do \
		if [ -f $$x ] ; then \
			cp $$x $@/ ; \
            echo "(build-utils:jar-file \"$$(basename $$x .jar)\")" >> $@/java.libs.asd ; \
		fi ; \
	done

	echo "))" >> $@/java.libs.asd


upload-sdk: .PHONY $(LW)
	$(LW_SCRIPT) scripts/upload-sdk.lisp

upload-mac-intel-sdk:
	ARCH="x86_64" make upload-sdk
