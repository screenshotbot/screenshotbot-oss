;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/test-magick-lw
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/magick-lw
                #:map-non-alpha-pixels
                #:magick-exception-message
                #:magick-read-image
                #:new-magick-wand
                #:magick-exception
                #:with-wand
                #:compare-images
                #:magick-native)
  (:import-from #:screenshotbot/magick
                #:convert-to-lossless-webp)
  (:import-from #:screenshotbot/magick-lw
                #:check-boolean)
  (:import-from #:util/digests
                #:md5-file)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/test-magick-lw)


(util/fiveam:def-suite)

(def-fixture state ()
  (tmpdir:with-tmpdir (tmpdir)
    (let ((rose (asdf:system-relative-pathname :screenshotbot "fixture/rose.png"))
          (rose-webp (asdf:system-relative-pathname :screenshotbot "fixture/rose.webp"))
          (wizard (asdf:system-relative-pathname :screenshotbot "fixture/wizard.png"))
          (transparent (asdf:system-relative-pathname :screenshotbot "fixture/point.png")))
      (&body))))

(test simple-file-load-save
  (with-fixture state ()
   (with-wand (wand :file rose)
     (pass))))

(test compare-nil
  (with-fixture state ()
    (with-wand (wand1 :file rose)
      (with-wand (wand2 :file wizard)
        (is-false (compare-images wand1 wand2))))))

(test compare-is-true
  (with-fixture state ()
    (with-wand (wand1 :file rose)
      (with-wand (wand2 :file rose-webp)
        (is-true (compare-images wand1 wand2))))))

(test convert-to-webp
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname out :type "webp")
      (convert-to-lossless-webp
       (make-instance 'magick-native)
       rose out)
      (with-wand (rose1 :file rose)
        (with-wand (out1 :file out)
          (is-true (compare-images rose1 out1)))))))

(test ensure-convert-to-webp-is-deterministic
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname out :type "webp")
      (convert-to-lossless-webp (make-instance 'magick-native)
                                rose out)
      (uiop:with-temporary-file (:pathname out2 :type "webp")
        (convert-to-lossless-webp (make-instance 'magick-native)
                                  rose out2)
        (is (equalp (md5-file out)
                    (md5-file out2)))))))

(test raises-magick-exception
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname p)
      (with-wand (wand)
        (handler-case
            (let ((code (magick-read-image wand (namestring p))))
              (check-boolean code wand)
              (fail "Excepted exception"))
          (magick-exception (e)
            (is (str:containsp "decode delegate for"
                               (magick-exception-message e)))))))))


(test find-first-non-transparent
  (handler-bind ((error (lambda (E)
                          (trivial-backtrace:print-backtrace e))))
   (with-fixture state ()
     (let ((transparent-pixel-for-null
             (block top
               (with-wand (wand :file transparent)
                 (map-non-alpha-pixels wand
                                       (lambda (i j)
                                         (return-from top (cons i j))))))))
       (is
        (equal (cons 20 30)
               transparent-pixel-for-null)))
     (is
      (equal (cons 0 0)
             (block top
               (with-wand (wand :file rose)
                 (map-non-alpha-pixels wand
                                       (lambda (i j)
                                         (return-from top (cons i j)))))))))))
