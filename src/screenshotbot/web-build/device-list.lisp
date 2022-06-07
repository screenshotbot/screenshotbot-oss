;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/web-build/device-list
  (:use #:cl)
  (:nicknames :screenshotbot/pro/web-build/device-list)
  (:local-nicknames (#:a #:alexandria))
  (:export
   #:*device-list*))
(in-package :screenshotbot/web-build/device-list)

(defun %%build-device-list ()
  "Build the list of emulation devices, by taking it from the local
  Google Chrome installation. NOTE THAT THIS IS NOT MEANT TO BE RUN IN
  PROD!"
  (let ((file "~/.config/google-chrome/Default/Preferences"))
    (let ((data (json:decode-json-from-string (uiop:read-file-string file))))
      (let ((items (json:decode-json-from-string
                    (a:assoc-value
                     (a:assoc-value (a:assoc-value data :devtools) :preferences)
                     :standard-emulated-device-list))))
        (loop for item in items
              collect (a:assoc-value item :title))))))


(defparameter *device-list*
  (list "iPhone SE"
        "iPhone XR"
        "iPhone 12 Pro"
        "Pixel 3 XL"
        "Pixel 5"
        "Samsung Galaxy S8+"
        "Samsung Galaxy S20 Ultra"
        "iPad Air"
        "iPad Mini"
        "Surface Pro 7"
        "Surface Duo"
        "Galaxy Fold"
        "Samsung Galaxy A51/71"
        "Nest Hub Max"
        "Nest Hub"
        "iPhone 4"
        "iPhone 5/SE"
        "iPhone 6/7/8"
        "iPhone 6/7/8 Plus"
        "iPhone X"
        "BlackBerry Z30"
        "Nexus 4"
        "Nexus 5"
        "Nexus 5X"
        "Nexus 6"
        "Nexus 6P"
        "Pixel 2"
        "Pixel 2 XL"
        "Pixel 3"
        "Pixel 4"
        "LG Optimus L70"
        "Nokia N9"
        "Nokia Lumia 520"
        "Microsoft Lumia 550"
        "Microsoft Lumia 950"
        "Galaxy S III"
        "Galaxy S5"
        "Galaxy S8"
        "Galaxy S9+"
        "Galaxy Tab S4"
        "JioPhone 2"
        "Kindle Fire HDX"
        "iPad Mini"
        "iPad"
        "iPad Pro"
        "Blackberry PlayBook"
        "Nexus 10"
        "Nexus 7"
        "Galaxy Note 3"
        "Galaxy Note II"
        "Moto G4"))
