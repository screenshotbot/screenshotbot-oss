(defpackage :screenshotbot/test-uname
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/uname
                #:cpu-codename
                #:parse-t-type
                #:uname-arch
                #:uname-kernel-version
                #:uname-os
                #:uname
                #:parse-uname))
(in-package :screenshotbot/test-uname)


(util/fiveam:def-suite)

(def-fixture state ()
  (&body))

(test simple-parsing ()
  (let ((uname (parse-uname "Darwin SVRNX6YQGQYR7 24.4.0 Darwin Kernel Version 24.4.0: Wed Mar 19 21:18:03 PDT 2025; root:xnu-11417.101.15~1/RELEASE_ARM64_T8112 arm6")))
    (is (typep uname 'uname))
    (is (equal "Darwin" (uname-os uname)))
    (is (equal "24.4.0" (uname-kernel-version uname)))
    (is (equal "arm6" (uname-arch uname)))))

(test parse-linux ()
  (let ((uname (parse-uname "Linux thecharmer 6.1.0-29-amd64 #1 SMP PREEMPT_DYNAMIC Debian 6.1.123-1 (2025-01-02) x86_64 GNU/Linux")))
    (is (equal "Linux" (uname-os uname)))
    (is (equal "6.1.0-29-amd64" (uname-kernel-version uname)))
    (is (equal "x86_64" (uname-arch uname)))))

(test parse-mac-cpu-type
  (let ((uname (parse-uname "Darwin SVRNX6YQGQYR7 24.4.0 Darwin Kernel Version 24.4.0: Wed Mar 19 21:18:03 PDT 2025; root:xnu-11417.101.15~1/RELEASE_ARM64_T8112 arm6")))
    (is (equal "T8112" (parse-t-type uname)))
    (is (equal "M2" (cpu-codename uname)))))

(test parse-another-cpu-codename
  (let ((uname (parse-uname "Darwin SVRRKJ23YY4QX 24.4.0 Darwin Kernel Version 24.4.0: Wed Mar 19 21:17:35 PDT 2025; root:xnu-11417.101.15~1/RELEASE_ARM64_T6041 arm64")))
    (is (equal "T6041" (parse-t-type uname)))
    (is (equal "M4 Max" (cpu-codename uname)))))

(test do-some-parsing-on-a-bad-uname
  (let ((uname (parse-uname "Darwin blah blah")))
    (is (equal nil (parse-t-type uname)))
    (is (equal nil (cpu-codename uname)))
    (is (equal "blah" (uname-arch uname))))
  
  (let ((uname (parse-uname "blah blah")))
    (is (equal nil (parse-t-type uname)))
    (is (equal nil (cpu-codename uname)))
    (is (equal nil (uname-arch uname)))))


(test apple-vm
  (let ((uname (parse-uname "Darwin VM-ba26e239df 23.6.0 Darwin Kernel Version 23.6.0: Mon Jul 29 21:13:03 PDT 2024; root:xnu-10063.141.2~1/RELEASE_ARM64_VMAPPLE arm64")))
    (is (equal "VMAPPLE" (parse-t-type uname)))))
