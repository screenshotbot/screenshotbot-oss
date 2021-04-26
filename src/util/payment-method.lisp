;; Copyright 2018-Present Modern Interpreters Inc.
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(in-package #:net.mfiano.lisp.stripe)

(define-object payment-method ()
  id
  billing-details
  customer
  card
  (type :reader payment-method-type))

(defmethod initialize-instance :after ((instance payment-method) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key card &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :card (make-instance 'card :data card))))

(define-query list-payment-methods (:type list)
  (:get "payment_methods")
  customer
  type)

(define-query detach-payment-method ()
  (:post "payment_methods/~a/detach" payment-method))

(define-query attach-payment-method ()
  (:post "payment_methods/~a/attach" payment-method)
  customer)

(define-query retrieve-payment-method (:type payment-method)
  (:get "payment_methods/~a" payment-method))

(define-query create-payment-method (:type payment-method)
  (:post "payment_methods")
  type
  card)
