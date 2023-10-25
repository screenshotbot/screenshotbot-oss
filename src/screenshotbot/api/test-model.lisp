(defpackage :screenshotbot/api/test-model
  (:use #:cl
        #:fiveam)
  (:import-from #:screenshotbot/api/model
                #:failed-run
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
                #:contains-string)
  (:local-nicknames (#:dto #:screenshotbot/api/model)))
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

(test decoding-list
  (let ((ret
         (decode-json (str:trim " [{\"version\": 2},{\"version\":3}] ")
                      '(:list version))))
    (is (listp ret))
    (is (equal 2 (slot-value (car ret) 'version)))
    (is (equal 3 (slot-value (cadr ret) 'version)))))

(test unrecognized-fields
  (let ((ret
         (decode-json (str:trim " {\"foo\": \"car\", \"version\": 2, \"bleh\": \"ten\"} ")
                      'version)))
    (is (typep ret 'version))
    (is (equal 2 (slot-value ret 'version)))))

(test missing-fields-is-initformed
  (let ((ret
         (decode-json (str:trim " {\"foo\": \"car\"} ")
                      'failed-run)))
    (is (typep ret 'failed-run))
    (is (equal nil (slot-value ret 'dto::id))))
  (let ((ret
          (decode-json (str:trim " {\"foo\": \"car\"} ")
                       'version)))
    (is (typep ret 'version))
    (is-false (slot-boundp ret 'version))))

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

(test can-encode-empty-screenshot-list
  (let ((run (make-instance 'dto:run
                            :screenshots nil)))
    (let ((output (with-output-to-string (out)
                    (yason:encode run out))))
      (assert-that output
                   (contains-string "\"screenshots\":[]")))))


(test can-encode-run-tags
  (let ((Run (make-instance 'dto:run
                            :tags (list "Foo" "bar"))))
    (finishes
      (with-output-to-string (out)
        (yason:encode run out))))

    (let ((Run (make-instance 'dto:run
                            :tags nil)))
      (finishes
        (with-output-to-string (out)
          (yason:encode run out)))))
