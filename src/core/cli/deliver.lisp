;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :core/cli/deliver
  (:use #:cl)
  (:export
   #:deliver-end-user-cli))
(in-package :core/cli/deliver)

(defun safe-delete-file (x)
  (when (uiop:file-exists-p x)
    (delete-file x)))

(defun fake-lw-find-dpsec-location (frame)
  nil)

(defun win-ssl-dir ()
  "C:/Program Files/OpenSSL-Win64/")

(defvar *libssl* (util/native-module:make-system-module
                     'openssl
                     :file-name (path:catfile (win-ssl-dir)
                                              "libssl-3-x64.dll")))

(defvar *libcrypto* (util/native-module:make-system-module
                     'openssl
                     :file-name (path:catfile (win-ssl-dir)
                                              "libcrypto-3-x64.dll")))

(defun embed-ssl ()
  (util/native-module:embed-module *libssl*)
  (util/native-module:embed-module *libcrypto*))

(defun write-embedded (name dir filename)
  (with-open-file (stream (path:catfile dir filename) :direction :output :if-exists :supersede)
    (write-sequence (fli:get-embedded-module-data name)
                    stream)))

(defun install-ssl ()
  (let ((dir (tmpdir:mkdtemp)))
    (setf util/native-module::*lib-cache-dir* dir)
    (util/native-module::load-embedded-module *libcrypto* :load nil)
    (util/native-module::load-embedded-module *libssl* :load nil)

    (comm:set-ssl-library-path 
     (list
      (path:catfile dir "libcrypto-3-x64.dll")
      (path:catfile dir "libssl-3-x64.dll")))

    (comm:ensure-ssl)))

#+lispworks
(defun include-template-file (template-builder)
  (let ((path (asdf:system-relative-pathname :screenshotbot.sdk "template.lisp")))
    (with-open-file (out path :direction :output
                              :if-exists :supersede)

      (fli:start-collecting-template-info)
      (funcall template-builder)
      (fli:print-collected-template-info :output-stream out))
    (format t "~%~%The FLI template content is: ~A~%~%~%" (uiop:read-file-string path))

    (let ((output (compile-file path)))
      (load output))))

(defun deliver-main (output-file template-builder-fn
                     &key restart-fn universal (level 5))
  (when (find-package :cl+ssl)
    (error "CL+SSL is present in this image, this can lead to problems when delivering this
on Mac. (e.g., the image will try to load libcrypto etc."))
  (when (find-package :bknr.cluster/server)
    (error "BKNR.CLUSTER is present in this image"))
  (unless universal
    (safe-delete-file output-file))

  #+lispworks
  (include-template-file template-builder-fn)

  #+mswindows
  (embed-ssl)

  (setf asdf:*central-registry* nil)
  (setf ql:*local-project-directories* nil)

  ;; Causes a warning early on from cl+ssl not finding libcrypto or
  ;; some such.
  (lw:undefine-action "When starting image" "Reset cl-mongo-id state")

  ;; See Lisp Support Call #43182
  ;; The goal is to allow us to run the CLI on Alpine Linux (or any linux that
  ;; is shipped with musl instead of glibc.)
  #+(and linux arm64)
  (setf (symbol-function 'sys::set-signals-mask) #'lw:false)

  (build-utils/deliver-script:default-deliver 
   (lambda ()
     #+mswindows
     (install-ssl)
     (funcall restart-fn))
   output-file
   level
   :keep-function-name t
   :keep-debug-mode (/= 0 level)
   #+mswindows :console #+mswindows :init
   #+mswindows :startup-bitmap-file #+mswindows nil
   :keep-clos :meta-object-slots
   :keep-pretty-printer t
   :keep-clos-object-printing t
   :kill-dspec-table nil ;; for defadvice, in particular clingon:exit
   :keep-lisp-reader t
   ;; temporary: get the build green
   :keep-symbols `(system:pipe-exit-status
                   dspec:find-dspec-locations)
   :packages-to-keep-symbol-names :all
   :multiprocessing t))



(defun deliver-end-user-cli (&key deliver-script
                               restart-fn
                               (universal #+darwin t)
                               output-file
                               (level 5)
                               template-builder-fn)
  (setf (fdefinition 'sentry-client::lw-find-dspec-location)
        #'fake-lw-find-dpsec-location)
  (format t "We'll be writing to ~A~%" output-file)

  (ensure-directories-exist output-file)

  (flet ((call-next ()
           (deliver-main output-file template-builder-fn
                         :level level
                         :restart-fn restart-fn
                         :universal universal)))
    (cond
     (universal
      (cond
       ((hcl:building-universal-intermediate-p)
        (call-next))
       (t
        (safe-delete-file output-file)
        (hcl:save-universal-from-script deliver-script))))
     (t
      (call-next)))))
