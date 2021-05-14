(defsystem :azula
    :serial t
    :depends-on (:str
                 :log4cl
                 :cl-fad)
    :components ((:file "main")
                 (:file "js")
                 (:file "build-file-env")
                 (:file "scanner")
                 (:file "all")))

(defsystem :azula/tests
    :serial t
    :depends-on (:azula))
