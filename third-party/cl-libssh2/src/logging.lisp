;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :libssh2)

(deflogger ssh2 ()
  :runtime-level +info+
  :compile-time-level +debug+
  :documentation "The logger for this library, use the macros
  ssh2.{dribble|debug|info|warn|error|fatal} for logging.

To increase the verbosity:

 (setf (hu.dwim.logger::log-level (hu.dwim.rdbms::find-logger 'libssh2::ssh2)) hu.dwim.logger:+debug+)")
