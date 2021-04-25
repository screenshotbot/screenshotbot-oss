;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;; these quickloads are required because we need to load them before
;; we set dspec:*redefinition-action* to :error
(ql:quickload "babel" :silent t)
(ql:quickload "clsql" :silent t)
(ql:quickload "clsql-helper" :silent t)
(ql:quickload "colorize" :silent t)
(ql:quickload "tmpdir" :silent t)


#+lispworks
(ql:quickload :osicat :silent t)

#+lispworks
(setf dspec:*redefinition-action* :error)

#+ccl
(progn
  (ql:quickload :util) ;; got to do this
  (funcall (find-symbol "JVM-INIT-FOR-CCL" "UTIL"))
  (setf *debugger-hook* nil))


(let ((system (or
               #+lispworks
               (let ((pos (position "-system" system:*line-arguments-list* :test 'equal)))
                 (when pos
                   (elt system:*line-arguments-list* (1+ pos))))
               "web.all.tests")))
 (ql:quickload system))

;;(ql:quickload "auth")
;;(asdf:load-system "auth")


(defun main ()
  (tmpdir:with-tmpdir (tmpdir)
    (make-instance 'util:safe-mp-store
                   :directory tmpdir
                   :subsystems (list (make-instance
                                      'bknr.datastore:store-object-subsystem)
                                     (make-instance
                                      'bknr.datastore:blob-subsystem)))
    #+(or ccl lispworks)
    (util:jvm-init)
    (fiveam:test foo-bar
      (fiveam:is-true (equal "foo" "foo")))
    (if (not (fiveam:run-all-tests))
        (uiop:quit 1)))
  (uiop:quit 0))


#+lispworks
(mp:initialize-multiprocessing :main nil #'main)

#-lispworks
(main)
