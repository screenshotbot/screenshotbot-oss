;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :encrypt/test-encrypt
  (:use #:cl
        #:fiveam
        #:encrypt)
  (:import-from #:encrypt
                #:get-blowfish-cipher
                #:*blowfish-cipher*
                #:blowfish-key)
  (:local-nicknames (#:a #:alexandria)))
(in-package :encrypt/test-encrypt)

(util/fiveam:def-suite)

(def-fixture state (&key dir)
  (tmpdir:with-tmpdir (tmpdir)
    (let ((util/store:*object-store* (namestring (or dir tmpdir))))
      (let ((old-cipher *blowfish-cipher*))
        (unwind-protect
             (progn
               (setf *blowfish-cipher* nil)
               (&body))
          (setf *blowfish-cipher* old-cipher))))))

(test blowfish-key-happy-path
  (with-fixture state ()
    (is (arrayp (blowfish-key)))))

(test blowfish-key-is-always-the-same
  (with-fixture state ()
    (is (equalp (blowfish-key)
                (blowfish-key)))))

(test multithreaded-key-is-always-the-same
  (with-fixture state ()
   (let ((num-threads 100)
         (correct 0)
         (incorrect nil)
         (lock (bt:make-lock))
         (store util/store:*object-store*))
     (let ((encrypted (encrypt "foobar")))
       (flet ((thread-call ()
                (let* ((util/store:*object-store* store)
                       (*cipher* nil)
                       (test-encryption (encrypt "foobar")))
                  (log:info "Store is: " util/store:*object-store*)
                  (cond
                    ((equal test-encryption encrypted)
                     (bt:with-lock-held (lock)
                       (incf correct)))
                    (t
                     (bt:with-lock-held (lock)
                       (push test-encryption incorrect)))))))
         (thread-call)
         (thread-call)
         (let ((threads (loop for i from 2 below num-threads
                             collect (bt:make-thread
                                      (lambda ()
                                        (ignore-errors
                                         (thread-call)))))))
           (loop for thread in threads
                do (bt:join-thread thread))
           (is (equal nil incorrect))
           (is (eql num-threads correct))))))))

(test encode-decode
  (with-fixture state ()
    (is (not (equal "foobar" (encrypt "foobar"))))
    (is (stringp (encrypt "foobar")))
    (is (equal "foobar" (decrypt (encrypt "foobar"))))))

(test encryption-always-results-in-the-same-output
  (let ((expected nil))
    (tmpdir:with-tmpdir (tmp)
      (with-fixture state (:dir tmp)
        (setf expected (encrypt "foobar")))
      (with-fixture state (:dir tmp)
        (is (equal expected (encrypt "foobar")))))))

(test padding
  (with-fixture state ()
    (is (equal "foo" (decrypt (encrypt "foo"))))))

(test |we're actually encrypting!|
  (with-fixture state ()
    (is (not (equalp
              (flexi-streams:string-to-octets "foo")
              (subseq (base64:base64-string-to-usb8-array (encrypt "foo") :uri t) 0 3))))))


(test minimum-encrypted-length
  (with-fixture state ()
    (is (<= 8 (length (str:replace-all "=" "" (encrypt "fo")))))))


(test encrypt-mongoid
  (with-fixture state ()
    (let ((mongoid (mongoid:oid)))
      (let ((encrypted (encrypt-mongoid mongoid)))
        (is (stringp encrypted))
        (is (eql 24 (length encrypted)))
        (is (equalp mongoid (decrypt-mongoid encrypted)))))))

(test decrypt-old-format
  (with-fixture state ()
    (cl-mock:with-mocks ()
      (let ((key (make-array 8 :element-type '(unsigned-byte 8)
                               :initial-contents #(1 2 3 4 5 6 7 8)))
            (mongoid (ironclad:hex-string-to-byte-array
                     "6335a9540900bb3e0bed4375")))
       (cl-mock:if-called
        'get-blowfish-cipher
        (lambda ()
          (ironclad:make-cipher 'ironclad:blowfish
                                 :mode :ecb
                                 :key key)))
        ;; It's okay to remove this next line if we change the format,
        ;; just keep the test after this.
        (is (equal "aZ5wagzYQY5Xwt9lowj1JA.."
                   (encrypt-mongoid mongoid)))
        (is (equalp
             mongoid
             (decrypt-mongoid "aZ5wagzYQY5Xwt9lowj1JA..")))))))
