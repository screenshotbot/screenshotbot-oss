
(defsystem :nibble
  :serial t
  :depends-on (:log4cl
               :util
               :auth
               :hunchentoot-extensions
               :secure-random)
  :components ((:file "package")
               (:file "nibble")))
