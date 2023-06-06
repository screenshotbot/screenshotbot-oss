;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/test-magick-lw
  (:use #:cl
        #:fiveam
        #:fiveam-matchers
        #:screenshotbot/mask-rect-api)
  (:import-from #:screenshotbot/magick/magick-lw
                #:magick-bad-exif-data
                #:magick-identify-image
                #:with-pixel
                #:magick-get-image-width
                #:magick-get-image-height
                #:resize-image
                #:save-as-webp
                #:pixel-set-color
                #:pixel-set-alpha
                #:set-wand-alpha-channel
                #:get-non-alpha-pixels
                #:y
                #:pixel
                #:x
                #:screenshotbot-set-pixel
                #:magick-new-image
                #:with-pixel-wand
                #:compare-wands
                #:magick-set-size
                #:verify-magick
                #:load-magick-native
                #:screenshotbot-verify-magick
                #:with-image-comparison
                #:ping-image-metadata
                #:map-non-alpha-pixels
                #:magick-exception-message
                #:magick-read-image
                #:new-magick-wand
                #:magick-exception
                #:with-wand
                #:check-boolean
                #:compare-images
                #:magick-native)
  (:import-from #:screenshotbot/magick/magick
                #:convert-to-lossless-webp)
  (:import-from #:util/digests
                #:md5-file)
  (:import-from #:fiveam-matchers/described-as
                #:described-as)
  (:import-from #:easy-macros
                #:def-easy-macro)
  (:import-from #:fiveam-matchers/strings
                #:matches-regex)
  (:local-nicknames (#:a #:alexandria)
                    #-lispworks
                    (#:fli #:util/fake-fli)))
(in-package :screenshotbot/magick/test-magick-lw)


(util/fiveam:def-suite)

(eval-when (:compile-toplevel :execute)
  (defun fixture (name)
    (asdf:system-relative-pathname :screenshotbot.magick
                                   (format nil "../fixture/~a" name))))

(def-fixture state ()
  (tmpdir:with-tmpdir (tmpdir)
    (let ((rose #.(fixture "rose.png"))
          (rose-webp #.(fixture "rose.webp"))
          (wizard #.(fixture "wizard.png"))
          (transparent #.(fixture "point.png")))
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


(test raises-magick-exception-message-is-not-corrupted
  ;; Just ensuring that magick-relinquish-memory is the right thing to
  ;; do.
  (with-fixture state ()
    (uiop:with-temporary-file (:pathname p)
      (with-wand (wand)
        (let ((messages
                (loop for x from 0 to 100
                      collect
                      (handler-case
                          (let ((code (magick-read-image wand (namestring p))))
                            (check-boolean code wand)
                            (fail "Excepted exception"))
                        (magick-exception (e)
                          (magick-exception-message e))))))
          (assert-that messages
                       (every-item
                        (starts-with "no decode delegate for"))))))))

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
      ;; Is this going to be constant? I hope so. If this changes with
      ;; ImageMagick version, then just replace this, okay?
      (equal (cons 244 38)
             (block top
               (with-wand (wand :file "logo:")
                 (map-non-alpha-pixels wand
                                       (lambda (i j)
                                         (return-from top (cons i j)))))))))))
(test ping-image-metadata
  (with-fixture state ()
    (is (equal '(70 46 "PNG")
               (ping-image-metadata (make-instance 'magick-native)
                                    rose)))
    (is (equal '(70 46 "WEBP")
               (ping-image-metadata (make-instance 'magick-native)
                                    rose-webp)))))

(test no-background-in-compare
  (with-fixture state ()
    (with-wand (one :file rose)
      (with-wand (two :file rose)
        (with-image-comparison (one two :result result :same-p same-p)
          (is-true same-p)
          (let ((non-alphas 0))
            (map-non-alpha-pixels result
                                  (lambda (x y)
                                    (incf non-alphas)))
            (assert-that non-alphas
                         (described-as "We shouldn't have a background image in the comparison"
                           (equal-to 0)))))))))

(test verify-magick-native
  (load-magick-native)
  (with-fixture state ()
    (finishes
      (verify-magick))))

(test force-reload-magick-native
  (load-magick-native)
  (finishes
   (load-magick-native :force t)))

;; Dummy test to look at the test output in builds to see which
;; version of magick is being used.
(test #+magick-6 using-magick-6 #+magick-7 using-magick7
  (pass))

(def-easy-macro with-large-wand (&binding wand &key (height 16385)  &fn fn)
  (with-wand (wand)
    (with-pixel-wand (pwand)
      (check-boolean
       (magick-new-image wand
                         1 height
                         pwand)
       wand))
    (funcall fn wand)))

#-screenshotbot-oss
(test large-image
  "If this test fails, definitely look at the global policy.xml for the
 given ImageMagick version. Currently, we can't override the
 resource:height policy, which by default is set to 16KP, at least on
 ImageMagick 6. On IM7, it appears to be set to 300KP, which I'm not
 sure if I changed myself in my local instance. On the docker
 instances it doesn't look like the height policy is set, and I'm too
 lazy to figure out what the default is. "
  (with-large-wand (before)
    (with-large-wand (after)
      (uiop:with-temporary-file (:pathname output :type "webp")
        ;; This does not work in Magick-6. I don't know why. Ideally
        ;; I'd like to make it work, but for now I'll just disable
        ;; this test. Essentially, expect compare-wands to fail on
        ;; large images in magic-6.
        (finishes
         (compare-wands before after output))))))

(test small-image-comparison-happy-path
  (with-large-wand (before :height 20)
    (with-large-wand (after :height 20)
      (uiop:with-temporary-file (:pathname output :type "webp")
        (finishes
          (compare-wands before after output))))))

(defun  mark-pixel (wand x y)
  (with-pixel (pixel x y)
    (check-boolean
     (screenshotbot-set-pixel
      wand
      pixel
      "rgba(10,0,0,1.0)")
     wand)))

(def-easy-macro with-single-pixel-image (&key &binding wand
                                              width
                                              height
                                              default-mark
                                              &fn fn)
  (with-wand (wand)
    (with-pixel-wand (pwand)
      (pixel-set-color pwand "none")
      (check-boolean
       (magick-new-image wand
                         width height
                         pwand)
       wand)
      (set-wand-alpha-channel wand))
    (when default-mark
      (mark-pixel wand 10 19))
    (fn wand)))

(def-fixture get-non-alpha (&key (width 40) (height 30) (default-mark t))
  #+nil
  (progn
    (asdf:perform
     'asdf:load-op
     (asdf:find-component :screenshotbot.magick "magick-native"))
    (load-magick-native :force t))
  (with-single-pixel-image (:wand wand :height height :width width
                            :default-mark t)
    (&body)))

(test get-non-alpha-pixels ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand)))
      (is (equalp (list 1 2)
                  (array-dimensions res)))
      (is
       (equalp #2A((10 19))
               res)))))

(defclass simple-mask ()
  ((left :initarg :left
         :reader mask-rect-left)
   (top :initarg :top
        :reader mask-rect-top)
   (width :initarg :width
          :reader mask-rect-width)
   (height :initarg :height
           :reader mask-rect-height)))

(test get-non-alpha-with-masks ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list (make-instance 'simple-mask
                                                          :left 0
                                                          :top 0
                                                          :height 3
                                                          :width 3)))))
      (is (equalp (list 1 2)
                  (array-dimensions res)))
      (is
       (equalp #2A((10 19))
               res)))))


(test get-non-alpha-with-masks-covering-change ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list (make-instance 'simple-mask
                                                          :left 9
                                                          :top 18
                                                          :height 6
                                                          :width 6)))))
      (is (equalp (list 0 2)
                  (array-dimensions res)))
      (pass))))

(test get-non-alpha-with-two-masks ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                          :left 0
                                                          :top 0
                                                          :height 3
                                                          :width 3)
                                      (make-instance 'simple-mask
                                                          :left 9
                                                          :top 18
                                                          :height 6
                                                          :width 6)))))
      (is (equalp (list 0 2)
                  (array-dimensions res))))))

(test get-non-alpha-with-masks-that-extend-the-space ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                          :left 0
                                                          :top 0
                                                          :height 300
                                                          :width 300)))))
      (is (equalp (list 0 2)
                  (array-dimensions res))))))

(test get-non-alpha-with-masks-left-border-is-included ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                          :left 10
                                                          :top 0
                                                          :height 300
                                                          :width 300)))))
      (is (equalp (list 0 2)
                  (array-dimensions res))))
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                     :left 11
                                                     :top 0
                                                     :height 300
                                                     :width 300)))))
      (is (equalp (list 1 2)
                  (array-dimensions res))))))

(test get-non-alpha-with-masks-top-border-is-included ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                          :left 0
                                                          :top 19
                                                          :height 300
                                                          :width 300)))))
      (is (equalp (list 0 2)
                  (array-dimensions res))))

    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                     :left 0
                                                     :top 20
                                                     :height 300
                                                     :width 300)))))
      (is (equalp (list 1 2)
                  (array-dimensions res))))))

(test get-non-alpha-with-masks-right-border-is-exclusive ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                          :left 0
                                                          :top 0
                                                          :width 11
                                                          :height 30)))))
      (is (equalp (list 0 2)
                  (array-dimensions res))))

    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                     :left 0
                                                     :top 0
                                                     :width 10
                                                     :height 100)))))
      (is (equalp (list 1 2)
                  (array-dimensions res))))))


(test get-non-alpha-with-masks-right-border-is-exclusive-even-with-overlaps ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                          :left 0
                                                          :top 0
                                                          :width 11
                                                          :height 30)
                                      (make-instance 'simple-mask
                                                     :left 3
                                                     :top 0
                                                     :width 5
                                                     :height 30)))))
      (is (equalp (list 0 2)
                  (array-dimensions res))))

    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                     :left 3
                                                     :top 0
                                                     :width 5
                                                     :height 30)
                                      (make-instance 'simple-mask
                                                     :left 0
                                                     :top 0
                                                     :width 10
                                                     :height 100)))))
      (is (equalp (list 1 2)
                  (array-dimensions res))))))


(test get-non-alpha-with-masks-bottom-border-is-exclusive ()
  (with-fixture get-non-alpha ()
    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                          :left 0
                                                          :top 0
                                                          :width 11
                                                          :height 20)))))
      (is (equalp (list 0 2)
                  (array-dimensions res))))

    (let ((res (get-non-alpha-pixels wand
                                     :masks
                                     (list
                                      (make-instance 'simple-mask
                                                     :left 0
                                                     :top 0
                                                     :width 11
                                                     :height 19)))))
      (is (equalp (list 1 2)
                  (array-dimensions res))))))

(test complex-case-from-prod
  (with-fixture get-non-alpha (:width 1500 :height 55)
    (let ((masks '((1243 22 9 26)
                   (1244 45 39 10)
                   (1280 18 8 31)
                   (1432 20 5 23)
                   (1479 21 8 21)
                   (1316 41 28 14)
                   (1437 52 46 10))))
      (mark-pixel wand 1261 50)
      (let ((res (get-non-alpha-pixels wand
                                       :masks
                                       (loop for mask in masks
                                             collect
                                             (make-instance 'simple-mask
                                                            :left (first mask)
                                                            :top (second mask)
                                                            :width (third mask)
                                                            :height (fourth mask))))))
        (is (equalp #2A((10 19))
                    res))))))

(test in-place-comparison
  (with-single-pixel-image (:wand wand :height 30 :width 30)
    (with-single-pixel-image (:wand wand2 :height 50 :width 50)
      (with-image-comparison (wand wand2 :in-place-p t
                                         :same-p same-p)
        (is-false same-p)))))

(test in-place-comparison-reverse-order
  (with-single-pixel-image (:wand wand2 :height 30 :width 30)
    (with-single-pixel-image (:wand wand :height 50 :width 50)
      (with-image-comparison (wand wand2 :in-place-p t
                                         :same-p same-p)
        (is-false same-p)))))

(test resize-never-upsizes-webp
  (with-single-pixel-image (:wand wand :height 10 :width 10)
    (uiop:with-temporary-file (:pathname input :type "webp")
      (save-as-webp wand input)
      (uiop:with-temporary-file (:pathname dest :type "webp")
        (resize-image input :output dest :size "30x20")
        (with-wand (res :file dest)
          (is (eql 10 (magick-get-image-height res)))
          (is (eql 10 (magick-get-image-width res))))))))


(test resize-never-upsizes-png
  (with-single-pixel-image (:wand wand :height 10 :width 10)
    (uiop:with-temporary-file (:pathname input :type "png")
      (save-as-webp wand input)
      (uiop:with-temporary-file (:pathname dest :type "webp")
        (resize-image input :output dest :size "30x20")
        (with-wand (res :file dest)
          (is (eql 10 (magick-get-image-height res)))
          (is (eql 10 (magick-get-image-width res))))))))

(test scales-both-dimensions
  (with-single-pixel-image (:wand wand :height 10 :width 30)
    (uiop:with-temporary-file (:pathname input :type "webp")
      (save-as-webp wand input)
      (uiop:with-temporary-file (:pathname dest :type "webp")
        (resize-image input :output dest :size "5x5")
        (with-wand (res :file dest)
          (is (eql 2 (magick-get-image-height res)))
          (is (eql 5 (magick-get-image-width res))))))))

(test identify-image
  (with-fixture state ()
    (with-wand (wand :file rose)
      (assert-that (magick-identify-image wand)
                   (matches-regex "png:IHDR.bit_depth: 8")))))

(test magick-bad-exif-data ()
  (with-fixture state ()
    (with-wand (wand :file rose)
      (is (fset:equal? (fset:empty-map)
                       (magick-bad-exif-data wand))))
    (with-wand (wand :file #. (fixture "image-with-timestamp.png"))
      (is (fset:equal?
           (fset:with
            (fset:empty-map)
            "png:tIME" "2023-06-06T14:13:20Z")
           (magick-bad-exif-data wand))))))
