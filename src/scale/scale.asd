(defsytem :scale
  :serial t
  :depends-on (:util/request
               :str)
  :components ((:file "core")
               (:file "linode")))
