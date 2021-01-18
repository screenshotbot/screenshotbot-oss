(in-package :cl-user)

(defpackage :bknr.utils
  (:use :cl
        :cl-ppcre
        :cl-interpol
        :md5
        #+sbcl :sb-ext
        #+openmcl :ccl)
  #+openmcl
  (:shadow :ccl #:copy-file #:make-process)
  (:shadowing-import-from :cl-interpol quote-meta-chars)
  (:export #:define-bknr-class

           ;; byte size formatting
           #:scale-bytes
   
           ;; date format
           #:format-date-time
           #:format-time-interval
           #:format-duration
           #:year-interval
           #:month-interval
           #:day-interval
           #:timetag
           #:daytag
           #:get-daytime
           #:get-hourtime
           #:get-monthtime
           #:previous-day
           #:next-day
           #:month-num-days

	   ;; shell functions
	   #:run-shell-command-to-string
	   #:run-shell-command
	   
           #:hostname
           #:parse-time

           ;; filesystem functions
           #:move-file
           #:directory-empty-p
           #:subdir-p
           #:make-temporary-pathname
           #:with-temporary-file
           #:file-contents
           #:parent-directory
   
           ;; list functions
           #:delete-first
           #:make-keyword-from-string

           #:assoc-values
           #:assoc-to-keywords
           #:insert-at-index
           #:find-neighbourhood
           #:group-by
           #:group-on
           #:find-all
           #:genlist
           #:nrotate
           #:shift-until
           #:count-multiple

           ;; hash table
           #:hash-to-list
           #:hash-values
           #:hash-keys
           #:incf-hash

           ;; randomize
           #:random-elts
           #:randomize-list

           ;; md5
           #:hash-to-hex
           #:md5-string

           ;; capabilty
           #:make-capability-string

           ;; content-types
           #:pathname-type-symbol
           #:image-content-type
           #:pathname-content-type
           #:image-type-symbol

           ;; utf-8
           #:convert-utf8-to-latin1

           ;; strings
           #:find-matching-strings
           #:make-extendable-string

           ;; stream
           #:read-delimited
           #:read-file

           ;; smbpasswd
           #:set-smb-password
           #:smb-password-error

           ;; actor
           #:bknr-actor
           #:bknr-actor-name
           #:run-function
           #:actor-start
           #:actor-stop
           #:actor-running-p

           ;; cron
           #:cron-actor

           ;; reader
           #:whitespace-char-p
           #:whitespace-p
           #:bknr-read-delimited-list
           #:bknr-read
           #:string-beginning-with-p
           #:string-delimited-by-p

           ;; crypt-md5
           #:crypt-md5
           #:verify-md5-password

           ;; FDF creation
           #:make-fdf-file

           #:remove-keys
           #:eval-initargs

           ;; Package cleaning for the build process
           #:within-temporary-package

           ;; mp compatibility
           #:mp-make-lock
           #:mp-with-lock-held
           #:mp-with-recursive-lock-held

           ;; class utils
           #:class-subclasses

           ;; norvig
           #:find-all

           ;; misc
           #:subseq*))
