include ../../../scripts/common.mk

CHRONIC=cd ../../../ &&
QEMUROOT=/opt/software/qemu
LWANDROIDROOT=/opt/software/lw-android
VERSION=8-0-0

MAIN=lispout/app/src/main
LIB=libscreenshotbot-showkase.so

DELIVER=src/screenshotbot/showkase/deliver-showkase-libs.lisp

.PHONY:

$(MAIN): .PHONY
	mkdir -p $@

$(TEST): .PHONY
	mkdir -p $@

lispout:$(MAIN) .PHONY
	touch $(MAIN)/AndroidManifest.xml
	touch lispout/build.gradle
	touch lispout/app/build.gradle

$(MAIN)/assets/$(LIB).x86_64.lwheap: lispout .PHONY
	@$(CHRONIC) $(LWIMAGEAMD64) -build $(DELIVER) && echo "Built: android/x86_64"
	test -f $@

$(MAIN)/jniLibs/x86_64/$(LIB): $(MAIN)/assets/$(LIB).x86_64.lwheap


$(MAIN)/assets/$(LIB).x86.lwheap: lispout .PHONY
	@$(CHRONIC) $(LWIMAGEX86) -build $(DELIVER) && echo "Built: android/x86"


$(MAIN)/jniLibs/x86/$(LIB): $(MAIN)/assets/$(LIB).x86.lwheap


$(MAIN)/assets/$(LIB).armeabiv7a.lwheap: lispout .PHONY
	@$(CHRONIC) "$(qemubinarm32)" -L "$(QEMUROOT)/arm-linux-libs" "$(LWIMAGEARM)" -build $(DELIVER) && echo "Built: android/arm32" && echo "Built: android/arm32"


$(MAIN)/jniLibs/armeabi-v7a/$(LIB): $(MAIN)/assets/$(LIB).armeabiv7a.lwheap


$(MAIN)/assets/$(LIB).arm64v8a.lwheap: lispout .PHONY
	@$(CHRONIC) "$(qemubinarm64)" -L "$(QEMUROOT)/arm-linux-libs" "$(LWIMAGEARM64)" -build $(DELIVER) && echo "Built: android/arm64"

$(MAIN)/jniLibs/arm64-v8a/$(LIB): $(MAIN)/assets/$(LIB).arm64v8a.lwheap

$(MAIN)/jniLibs/x86_64/$(LIB): $(MAIN)/assets/$(LIB).x86_64.lwheap

all-except-x86_64: $(MAIN)/jniLibs/x86/$(LIB) $(MAIN)/jniLibs/arm64-v8a/$(LIB) $(MAIN)/jniLibs/armeabi-v7a/$(LIB)

x86_64: $(MAIN)/jniLibs/x86_64/$(LIB)

$(TEST)/jniLibs/x86_64/$(LIB): $(TEST)/assets/$(LIB).x86_64.lwheap

clean:
	rm -rf lispout
	rm -rf testout

test-imgs: $(TEST)/assets/$(LIB).x86_64.lwheap
