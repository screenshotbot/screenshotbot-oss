(defpackage :screenshotbot/model/image-comparison
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package :screenshotbot/model/image-comparison)

(ignore-errors
 (import (uiop:find-symbol* :image-comparison
                            :screenshotbot/dashboard/compare)))

(uiop:export* :image-comparison *package*)
