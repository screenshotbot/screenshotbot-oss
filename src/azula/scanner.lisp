(pkg:define-package :azula/scanner
  (:use :cl
   :alexandria)
  (:import-from ./main
   :azula-root
                :canonical-name
   :target-build-file
                :target-srcs
                :build-file-path
                :target-deps
                :build-file-pathname
                :load-build-file
                :*targets*))
(in-package :azula/scanner)

(defvar *current-build-file*)

(defvar *azula* "AZULA")

(defun join-azula-dirs (dir1 name)
  (cond
    ((equal dir1 "//")
     (format nil "~a~a" dir1 name))
    (t
     (format nil "~a/~a" dir1 name))))

(defun scan ()
  (let (new-targets)
   (let ((*targets* nil))
     (labels ((%scan (directory prefix)
                (log:debug "Scanning: ~S ~S" directory prefix)
                (let ((build-file (path:catfile directory *azula*)))
                  (when (path:-e build-file)
                    (load-build-file build-file prefix)))
                (unless (equal prefix "//build")
                  (dolist (dir (fad:list-directory directory))
                    (when (path:-d dir)
                      (%scan dir (join-azula-dirs prefix (car (last (pathname-directory dir))))))))))
       (%scan (azula-root) "//")
       (resolve-targets *targets*)
       (mapc 'resolve-srcs *targets*)
       (verify-no-loops *targets*)
       (setf new-targets *targets*)))

    (setf *targets* new-targets)))

(defun resolve-targets (targets)
  "replace all targets in the deps with the actual reference to the
  target."
  (let ((name-map (make-hash-table :test 'equal)))
    (dolist (target targets)
      (setf (gethash (canonical-name target) name-map)
            target))
    (flet ((%resolve-dep (build-file name)
             (let ((name
                     (cond
                       ((str:starts-with-p "//" name)
                        name)
                       ((str:starts-with-p ":" name)
                        (format nil "~a~a" (canonical-name build-file)
                                name))
                       (t
                        (error "Invalid dep format: ~a" name)))))
               (or
                (gethash name name-map)
                (error "Undefined target: ~a" name))))
           )
      (dolist (target targets)
        (setf (target-deps target)
              (loop for dep in (target-deps target)
                    collect
                    (%resolve-dep (target-build-file target)
                                  dep)))))))

(defun verify-no-loops (targets)
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((visit (target)
               (setf (gethash target seen) t)
               (dolist (dep (target-deps target))
                 (when (gethash dep seen)
                   (error "Found cyclical dependency between: ~S and ~S"
                          (canonical-name target)
                          (canonical-name dep)))
                 (visit dep))))
      (dolist (target targets)
        (unless (gethash target seen)
          (visit target))))))

(defun resolve-srcs (target)
  (let ((build-file (target-build-file target)))
   (setf (target-srcs target)
         (loop for src in (target-srcs target)
               collect
               (let ((file (path:catfile (build-file-pathname build-file)
                                         src)))
                 (unless (path:-e file)
                   (error "File: ~A does not exist" file))
                 file)))))

;; (scan)
