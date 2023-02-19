(defpackage :screenshotbot/api/test-model
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/model
                #:screenshot
                #:screenshot-list
                #:decode-json
                #:encode-json
                #:version)
  (:import-from #:fiveam-matchers/lists
                #:contains)
  (:import-from #:fiveam-matchers/core
                #:is-equal-to
                #:has-typep
                #:assert-that)
  (:import-from #:fiveam-matchers/strings
                #:contains-string))
(in-package :screenshotbot/api/test-model)

(util/fiveam:def-suite)

#+nil
(test simple-encoding
  (let ((ret (encode-tree (make-instance 'version :version 20))))
    (is (equal "version" (gethash "_type" ret)))
    (is (equal 20 (gethash "version" ret)))))

(test simple-json-encoding
  (let ((ret (make-instance 'version :version 2)))
    (is (equal (str:trim " {\"version\":2} ")
               (encode-json ret)))))

(test simple-decoding
  (let ((ret
         (decode-json (str:trim " {\"version\": 2} ")
                      'version)))
    (is (typep ret 'version))
    (is (equal 2 (slot-value ret 'version)))))

(test unrecognized-fields
  (let ((ret
         (decode-json (str:trim " {\"foo\": \"car\", \"version\": 2, \"bleh\": \"ten\"} ")
                      'version)))
    (is (typep ret 'version))
    (is (equal 2 (slot-value ret 'version)))))


(test parse-screenshot-list
  (assert-that
   (json-mop:json-to-clos
    "[{\"name\":\"bleh\"}] "
    'screenshot-list)
   (contains (has-typep 'screenshot))))


(test can-encode-null-screenshot
  (let ((screenshot (make-instance 'screenshot
                                   :name "foo"
                                   :image-id "bleh"
                                   :lang nil)))
    (let ((str (with-output-to-string (out)
                 (yason:encode screenshot out))))
      (assert-that str
                   (contains-string "\"lang\":null")))))
