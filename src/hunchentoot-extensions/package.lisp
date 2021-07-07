;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :hunchentoot-extensions
  (:nicknames :hex)
  (:use #:cl
        #:alexandria)
  (:export #:base-acceptor
           #:better-easy-handler
           #:acceptor-funcall-handler
           #:make-full-url
           #:acceptor-plugin-name
           #:acceptor-plugin-prefix
           #:wrap-template
           #:acceptor-plugin
           #:define-plugin-handler
           #:acceptor-with-plugins
           #:log-crash-extras
           #:*acceptor-plugin*
           #:safe-domain
           #:make-uri-regex
           #:acceptor-db-config
           #:make-uri-regex
           #:add-get-param-to-url
           #:declare-handler
           #:acceptor-plugins
           #:safe-redirect
           #:make-url))
