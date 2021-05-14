(defpackage :azula/build-file-env
  (:use :cl)
  (:import-from :azula/main
                :define-target))
(in-package :azula/build-file-env)

;; This package is USEd in the default *PACKAGE* when reading an AZULA
;; file


(define-target js-library azula/js:js-library)

(define-target js-binary azula/js:js-binary)
