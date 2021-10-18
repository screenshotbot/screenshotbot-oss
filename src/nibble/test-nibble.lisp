(defpackage :nibble/test-nibble
  (:use #:cl
        #:fiveam)
  (:import-from #:nibble
                #:defnibble
                #:nibble-plugin
                #:nibble
                #:render-nibble)
  (:local-nicknames (#:a #:alexandria)))
(in-package :nibble/test-nibble)


(util/fiveam:def-suite)


(def-fixture state ()
  (let ((plugin (make-instance 'nibble-plugin
                                :prefix "/n/")))
    (&body)))

(test preconditions
  (with-fixture state ()
   (is (equal "foobar"
              (render-nibble
               plugin
               (nibble ()
                 "foobar"))))))


(defnibble foo ()
  "foobar2")

(test named-nibble
  (with-fixture state ()
    (is (equal "foobar2"
               (render-nibble
                plugin
                (nibble foo))))))

(defnibble foo-with-args (name)
  (format nil "hello ~a" name))

(test named-nibble-with-args
  (with-fixture state ()
    (util:with-fake-request (:script-name "/?name=arnold")
      (auth:with-sessions ()
       (is (equal "hello arnold"
                  (render-nibble
                   plugin
                   (nibble foo-with-args))))))))
