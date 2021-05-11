(defsystem :azula
  :serial t
  :components ((:file "main")
               (:file "all")))

(defsystem :azula/tests
  :serial t
  :depends-on (:azula))
