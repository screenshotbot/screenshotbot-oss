# Copyright 2018-Present Modern Interpreters Inc.
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

sbcl=build/sbcl-console
CACHE_KEY=4
SBCL_CORE=sbcl
CCL_DEFAULT_DIRECTORY=/opt/software/ccl
CCL_CORE=$(CCL_DEFAULT_DIRECTORY)/lx86cl64
CCL_IMAGE=build/ccl-console
tests= \
	./test-stuff.lisp \
	./test-stuff2.lisp

JIPR=../jippo
LW=build/lw-console
LW_CORE=lispworks-unknown-location
SRC_DIRS=src local-projects third-party
LISP_FILES=$(shell find $(SRC_DIRS) -name '*.lisp') $(shell find $(SRC_DIRS) -name '*.asd')
JAR_FILE=java/build/lib/java.jar
SO=$(JAR_FILE)
LW_SCRIPT=timeout 5m $(LW) -quiet -build
SBCL_SCRIPT=$(sbcl) --script
TMPFILE=$(shell mktemp)
JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
SBCL_SCRIPT=timeout 5m $(sbcl) --script
CCL_SCRIPT=CCL_DEFAULT_DIRECTORY=$(CCL_DEFAULT_DIRECTORY) $(CCL_CORE) -b -I $(CCL_IMAGE)

QUICKLISP=quicklisp/dists/quicklisp/
COPYBARA=java -jar scripts/copybara_deploy.jar

UNAME=$(shell uname -s)

REVISION_ID=$(shell echo '{"ids":["$(DIFF_ID)"]}' | arc call-conduit differential.querydiffs | jq -r '.["response"]["$(DIFF_ID)"]["revisionID"]')

ifeq ($(UNAME),Linux)
	LW_CORE=/opt/software/lispworks/lispworks-7-1-*
endif

ifeq ($(UNAME),Darwin)
	LW_CORE=/Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin
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

update-quicklisp: .PHONY
	$(SBCL_CORE) --eval '(load "~/quicklisp/setup.lisp")' --eval '(ql:update-all-dists :prompt nil)' --quit

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

$(JAR_FILE): $(shell find java | grep -v build | grep -v .gradle)
	cd java && JAVA_HOME=$(JAVA_HOME) ./gradlew build makeClasspath

test-sb: submodule $(SO) $(sbcl) $(JAR_FILE)
	pwd
	$(sbcl) --script ./jenkins.lisp

test-ccl: submodule $(SO) $(CCL_IMAGE) $(JAR_FILE)
	pwd
	$(CCL_SCRIPT) ./jenkins.lisp

test-lw: submodule $(SO) $(LW) $(JAR_FILE)
	pwd
	$(LW_SCRIPT) ./jenkins.lisp

test-store: submodule $(LW)
	$(LW_SCRIPT) ./run-store-tests.lisp

deploy-jipr:
	cd $(JIPR) && buck build jipr
	cd $(JIPR) && cp `buck targets --show-output jipr | cut -d ' ' -f 2`  $(PWD)/jipr/static/binary/jipr.jar

restart: | test-lw  web-bin
	kill -9 `curl https://tdrhq.com/deploy/getpid`

build: | build/cache-key build/distinfo.txt

web-bin: $(LISP_FILES) $(SO) $(LW)
	$(LW_SCRIPT) build-web-bin.lisp

clean-fasl: .PHONY
	find . -name *.64ufasl -delete

clean: clean-fasl
	rm -f web-bin
	cd shhhift/static && $(MAKE) clean

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

screenshotbot-tests: $(LW)
	$(LW_SCRIPT) ./jenkins.lisp -system screenshotbot/tests

deploy-rsync: .PHONY web-bin
	rsync -aPz --exclude .git --exclude .web-bin-copy --exclude web-bin ./ ubuntu@mx.tdrhq.com:~/web/


build/distinfo.txt: .PHONY
	mkdir -p build
	if test -e $(QUICKLISP)/distinfo.txt && ! ( test -e $@ && diff -q $@ $(QUICKLISP)/distinfo.txt ) ; then \
		cp $(QUICKLISP)/distinfo.txt $@ ; \
	fi

build/lw-console: build scripts/build-image.lisp
	$(LW_CORE) -build scripts/build-image.lisp

$(sbcl): build scripts/build-image.lisp
	$(SBCL_CORE) --script scripts/build-image.lisp


selenium-tests: $(LW) $(JAR_FILE)
	xvfb-run $(LW_SCRIPT) scripts/run-selenium-tests.lisp

selenium-tests-without-x: $(LW)
	$(LW_SCRIPT) scripts/run-selenium-tests.lisp

assets: $(LW) .PHONY
	mkdir -p assets
	$(LW_SCRIPT) scripts/deliver-screenshotbot.lisp

deploy-assets: | deploy-assets-excl-bin deploy-bin deploy-pull

deploy-assets-excl-bin: assets
	rsync -aPz --exclude screenshotbot assets web@screenshotbot.io:~/web/

deploy-bin: assets
	rsync -aPz assets/screenshotbot web@screenshotbot.io:~/web/assets/screenshotbot-copy
	ssh web@screenshotbot.io mv '~/web/assets/screenshotbot-copy' '~/web/assets/screenshotbot'

deploy-pull:
	ssh web@screenshotbot.io 'cd web && git pull'

$(CCL_IMAGE): build scripts/build-image.lisp
	rm -f $@
	$(CCL_CORE) -l scripts/build-image.lisp
	chmod a+x $@


update-ip: $(sbcl)
	$(SBCL_SCRIPT) update-ip.lisp

copybara: .PHONY
	# This is on arnold's jenkins server. Disregard for OSS use.
	ssh-add ~/.ssh/id_rsa_screenshotbot_oss
	$(COPYBARA) copy.bara.sky || true # avoid the no-op issue

conditional-copybara: validate-copybara
	if [ x$$DIFF_ID = x ] ; then \
		ssh-agent $(MAKE) copybara ; \
	fi

validate-copybara: .PHONY
	$(COPYBARA) validate copy.bara.sky

update-harbormaster-pass: $(sbcl)
	$(SBCL_SCRIPT) ./scripts/update-phabricator.lisp pass

update-harbormaster-fail: $(sbcl)
	$(SBCL_SCRIPT) ./scripts/update-phabricator.lisp fail


autoland:
	if ( echo "{\"revision_id\":\"$(REVISION_ID)\"}" | arc call-conduit differential.getcommitmessage | grep "#autoland" ) ; then \
	 	$(MAKE) -s actually-land ; \
	fi

actually-land:
	rm .buckconfig.local
	git status
	echo "Landing..."
	arc land  --keep-branch
