(defpackage :build-utils/common
  (:use #:cl)
  (:export #:find-transitive-system-deps))
(in-package :build-utils/common)

(defun all-transitive-deps (system)
  "Find all transitive deps in topological sort order. Uses Kahn's
  algorithm."
  (let ((L nil)
        (seen (make-hash-table :test 'equal)))
    (labels ((push-seen (x)
               #+nil
               (assert (stringp x))
               (setf (gethash x seen) t))
             (seenp (x)
               #+nil
               (assert (stringp x))
               (gethash x seen))
             (visit (n)
               (unless (seenp n)
                 (push-seen n)
                 (dolist (m (asdf:system-depends-on (asdf:find-system n)))
                   (visit m))
                 (push-seen n)
                 (push n L))))
      (visit (asdf:component-name system)))
    (nreverse L)))

(defun find-transitive-system-deps (system type)
  (let ((all-deps (all-transitive-deps system)))
    (loop for x in (mapcar 'asdf:find-system all-deps)
          if (typep x type)
            collect x)))
