;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :util/tests/test-testing
  (:use #:cl
        #:fiveam)
  (:import-from #:util/testing
                #:with-global-binding)
  (:local-nicknames (#:a #:alexandria)))
(in-package :util/tests/test-testing)


(util/fiveam:def-suite)

(defvar *var*)
(defvar *var2* :foo)

(test with-global-binding ()
  (with-global-binding ((*var* :bar))
    (is (eql :bar *var*)))
  (is (not (boundp '*var*)))
  (with-global-binding ((*var2* :bar))
    (is (eql :bar *var2*)))
  (is (eql :foo *var2*))
  (with-global-binding ((*var* 1)
                        (*var2* 2))
    (is (eql 1 *var*))
    (is (eql 2 *var2*)))
  (is (eql :foo *var2*))
  (is (not (boundp '*var*))))
