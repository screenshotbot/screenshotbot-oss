;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :auth/model/roles
  (:use #:cl))
(in-package :auth/model/roles)

;;;; See https://phabricator.tdrhq.com/w/user_roles/

(defclass role ()
  ())

(defclass read-only (role)
  ())

(defclass reviewer (role)
  ())

(defclass disabled-user (role)
  ())

(defclass standard-member (read-only reviewer)
  ())

(defclass integrations-developers (standard-member)
  ()
  (:documentation "Can access settings related to creating integrations"))

(defclass admin (standard-member)
  ())

(defclass owner (admin)
  ())

(defclass guest (read-only)
  ())

(defclass external-member (guest reviewer)
  ())

(defclass site-admin (owner)
  ())
