(defsystem :pixel-diff
  :serial t
  :depends-on (:screenshotbot.js-assets/lisp
               :util/misc)
  :components ((:file "differ")))
